## Objectives of this book

Bloc is the new graphics library for Pharo. A graphics library implies several aspects such as coordinate systems, drawing shape, clipping, and event management. 

In this tutorial, you will build a memory game. Given a provided model of a game, we will focus on creating a UI for it.

### Memory game

Let us have a look at what we want to build with you: a simple Memory game. 
In a memory game, players need to find pairs of similar cards. In each round, 
a player turns over two cards at a time. If the two cards show the same symbol 
they are removed and the player gets a point. If not, they are both returned facedown. 

For example, Figure *@figmemoryExample0@* shows the game after the first selection 
of two cards. Facedown cards are represented with a cross and turned cards show their number. 
Figure *@figmemoryExample1@* shows the same game after a few 
rounds. While this game can be played by multiple players, in this tutorial we will 
build a game with just one player. 

![The game after the player has selected two cards: facedown cards are represented with a cross and turned cards with their number.](figures/memoryExample0.png width=60&label=figmemoryExample0)

Our goal is to have a working game with a model and a simple graphical user interface. 
In the end, the following code should be able to build, initialize, and launch the game:

```
game := MGGame withNumbers.
visual := MGGameElement new.
visual memoryGame: game.	

space := BlSpace new. 
space extent: 420@420.
space root addChild: visual.
space show 
```


- First, we create a game model and ask you to associate the numbers from 1 to 8 with the cards. By default, a game model has a size of 4 by 4, which fits eight pairs of numbered cards.
- Second, we create a graphical game element.
- Third, we assign the model of the game to the UI. 
- Finally, we create and display a graphical space in which we place the game UI. Note that this last sequence should be better packaged as a message to the `MGGameElement`.

 
 
![Another state of the memory game after the player has correctly matched two pairs.](figures/memoryExample1.png width=60&label=figmemoryExample1)

### Getting started


This tutorial is for Pharo 11.0 \(`https://pharo.org/download`\) running on the latest compatible Virtual machine. You can get them at the following address: `http://www.pharo.org/`

To load Bloc, execute the following snippet in a Pharo Playground:

```
[ Metacello new
	baseline: 'Bloc';
	repository: 'github://pharo-graphics/Bloc:master/src';
	onConflictUseIncoming;
	ignoreImage;
	load ]
		on: MCMergeOrLoadWarning
		do: [ :warning | warning load ]
```


### Loading the Memory Game

To make the demo easier to follow and help you if you get lost, we already made a full implementation of the game. You can load it using the following code:

```
Metacello new
    baseline: 'BlocMemoryTutorial';
    repository: 'github://pharo-graphics/Bloc-Memory-Tutorial/src';
    load
```

After you have loaded the MemoryTutorial project, you will get two new packages: `Bloc-Memory` and `Bloc-MemoryGame-Demo`. `Bloc-MemoryTests` contains the full implementation of the game. 

You can browse a model of the game just executing the following code snippet:

```
MGGame withEmoji 
```

To get a 

```
MGGameElement openWithNumber
```


## Game model insights

Before starting with the actual graphical elements, we first need a model for our game.
This game model will be used as the Model in the typical Model View architecture.
On the one hand, the model does not communicate directly with the graphical elements;
all communication is done via announcements. On the other hand, the graphic elements are 
communicating directly with the model.

In the remainder of this chapter, we describe the game model in detail. If you want to move directly to
building graphical elements using Bloc, this model is fully defined in the package.


### Reviewing the card model

Let us start with the card model: a card is an object holding a symbol to be displayed, a state representing whether it is flipped or not, and an announcer that emits state changes. This object could also be a subclass of Model which already provides announcer management. 

```
Object << #MGCardModel
	slots: { #symbol . #flipped . #announcer};
	tag: 'Model';
	package: 'Bloc-Memory'
```



After creating the class we define an `initialize` method to set the card as not flipped, together with several accessors:

```
MGCardModel >> initialize
	super initialize.
	flipped := false
```

```
MGCardModel >> symbol: aCharacter
	symbol := aCharacter
```

```
MGCardModel >> symbol
	^ symbol
```

```
MGCardModel >> isFlipped
	^ flipped
```

```
MGCardModel >> announcer
	^ announcer ifNil: [ announcer := Announcer new ]
```


### Card simple operations


Next we need two methods to flip a card and make it disappear when it is no longer needed in the game.

```
MGCardModel >> flip
	flipped := flipped not.
	self notifyFlipped
```


```
MGCardModel >> disappear
	self notifyDisappear
```


### Adding notification

The notification is implemented as follows in the `notifyFlipped` and `notifyDisappear` methods. 
They simply announce events of type `MGCardFlippedAnnouncement` and `MGCardDisappearAnnouncement`. 
The graphical elements have to register subscriptions to these announcements as we will see later.

```
MGCardModel >> notifyFlipped
	self announcer announce: MGCardFlippedAnnouncement new
```


```
MGCardModel >> notifyDisappear
	self announcer announce: MGCardDisappearAnnouncement new
```


Here, `MGCardFlippedAnnouncement` and `MGCardDisappearAnnouncement` are subclasses of `Announcement`.

```
Announcement << #MGCardFlippedAnnouncement
	package: 'Bloc-Memory'
```


```
Announcement << #MGCardDisappearAnnouncement
	package: 'Bloc-Memory'
```


We add one final method to print a card more nicely and we are done with the card model!

```
MGCardModel >> printOn: aStream
	aStream
		nextPutAll: 'Card';
		nextPut: Character space;
		nextPut: $(;
		nextPut: self symbol;
		nextPut: $)
```


### Reviewing the game model

The game model is simple: it keeps track of all the available cards and all the cards currently selected by the player. 

```
Object << #MGGameModel
	slots: { #availableCards . #chosenCards};
	package: 'Bloc-MemoryGame-Demo-Model'
```


The `initialize` method sets up two collections for the cards.

```
MGGameModel >> initialize
	super initialize.
	availableCards := OrderedCollection new.
	chosenCards := OrderedCollection new
```

```
MGGameModel >> availableCards
	^ availableCards
```

The `chosenCards` collection will hold at max two cards in this version of the game. 

```
MGGameModel >> chosenCards
	^ chosenCards
```


### Grid size and card number

For now, we'll hardcode the size of the grid and the number of cards that need to be matched by a player.
Later this could be turned into an instance variable and be configured.

```
MGGameModel >> gridSize
	"Return grid size"
	^ 4
```

The method `matchesCount` indicates that two identical cards are needed to match. 

```
MGGameModel >> matchesCount
	"How many chosen cards should match in order for them to disappear"
	^ 2
```

```
MGGameModel >> cardsCount
	"Return how many cards there should be depending on grid size"
	^ self gridSize * self gridSize
```



### Initialization

To initialize the game with cards, we add an `initializeForSymbols:` method. 
This method creates a list of cards from a list of characters and shuffles it. 
We also add an assertion in this method to verify that the caller provided enough characters to fill up the game board.

```
MGGameModel >> initializeForSymbols: characters

	aCollectionOfCharacters size = (self cardsCount / self matchesCount)
		ifFalse: [ self error: 'Amount of characters must be equal to possible all combinations' ].

	aCollectionOfCharacters do: [ :aSymbol |
		1 to: self matchesCount do: [ :i |
		availableCards add: (MGCard new symbol: aSymbol) ] ].
	availableCards := availableCards shuffled
```


### Game logic

Next, we define the method `chooseCard:`. It will be called when a user selects a card. 
This method is the most complex method of the model and implements the main
logic of the game. 

- First, the method makes sure that the chosen card is not already selected.
This could happen if the view uses animations that give the player the chance to click on a card more than once.
- Next, the card is flipped by sending it the message `flip`. 
- Finally, depending on the actual state of the game, the step is complete and the selected cards are either removed or flipped back.

```
MGGameModel >> chooseCard: aCard
	(self chosenCards includes: aCard) 
		ifTrue: [ ^ self ].
	self chosenCards add: aCard.
	aCard flip.
	self shouldCompleteStep
		ifTrue: [ ^ self completeStep ].
	self shouldResetStep
		ifTrue: [ self resetStep ]
```


##### Completed. 
The current step is completed if the player selects the right amount of cards and they all show the same symbol.
In this case, all selected cards receive the message `disappear` and are removed from the list of selected cards.

```
MGGameModel >> shouldCompleteStep
	^ self chosenCards size = self matchesCount 
		and: [ self chosenCardMatch ]
```

```
MGGameModel >> chosenCardMatch
	| firstCard |
	firstCard := self chosenCards first.
	^ self chosenCards allSatisfy: [ :aCard | 
		aCard isFlipped and: [ firstCard symbol = aCard symbol ] ]
```
Note that the logic of chosenCardMatch looks more complex than expected but it works with matches that require more than two cards. 

```
MGGameModel >> completeStep
	self chosenCards 
		do: [ :aCard | aCard disappear ];
		removeAll.
```


##### Reset.

The current step should be reset if the player selects a third card. This will happen when a player already
selected two cards that do not match and clicks on a third one. In this situation, the two initial cards will be
flipped back. The list of selected cards will only contain the third card.

```
MGGameModel >> shouldResetStep 
	^ self chosenCards size > self matchesCount
```

```
MGGameModel >> resetStep
	| lastCard |
	lastCard := self chosenCards  last.
	self chosenCards 
		allButLastDo: [ :aCard | aCard flip ];
		removeAll;
		add: lastCard
```



### Ready 


We are now ready to start building the game view.
