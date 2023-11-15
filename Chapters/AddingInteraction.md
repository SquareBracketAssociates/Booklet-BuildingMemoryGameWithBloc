
## Adding Interaction


Now we will add interaction to the game. We want to flip the cards by clicking on them. 
Bloc supports such situations using two mechanisms: on one hand, event listeners handle events
and on the other hand, the communication between the model and view is managed via the registration to announcements
sent by the model.


### Adding events and event listeners

In Bloc, there is of course plenty of Events and we will focus on `BlClickEvent`. We can also say that events are easily managed through event handlers 

Now we should add an event handler to each card because we want to know which card will be clicked and pass this 
information to the game model.


```
MgdGameElement >> initialize

	super initialize.
	self size: 80 @ 80.
	self background: self backgroundPaint.
	self geometry:
		(BlRoundedRectangleGeometry cornerRadius: self cornerRadius).
	self drawBackSide.
	self layout: BlFrameLayout new.
	self addEventHandlerOn: BlClickEvent do: [ :anEvent | self click ]
```

We can easily see that whenever our card Element will receive a click Event, we will send the `click` message to this element


Please note, that if we created an instance of BlEventListener and added it as an event handler, we can reuse the same event handler for all card elements. It allows us to reduce overall memory consumption and improve game initialization time.

![Debugging the clickEvent: anEvent method.](figures/ClickWithDebugger.png width=100&label=figBoardFull)


Now the preview is not enough and we should create a window and embed the game element. 
Then when you click on a card you should get a debugger as shown in Figure *@figBoardFull@*.

### Specialize click

Now we can specialize the `click` method as follows: 
- We tell the model we just chose this card
- We draw our cardElement according to its card state 


```
MgdRawCardElement >> click

	self parent memoryGame chooseCard: self card.
	self drawCardElement
```


It means that the memory game model is changed but we cards don't flip back after mistaking the symbols. Indeed this is normal. We never made sure that visual elements were listening to model changes except for when we click on it.  This is what we will do in the following chapter. 



### Connecting the model to the UI

Now we show how the domain communicates with the user interface: the domain emits notifications
using announcements but it does not refer to the UI elements. It is the visual elements that should register to the notifications and react accordingly. We can prepare the message that will tell our elements to disappear we both cards match, otherwise we just tell our cards to flip back and draw their backside

```
MgdRawCardElement >> disappear
	"nothing for now"
```

Now we can modify the setter so that when a card model is set to a card graphical element, we register to the notifications emitted by the model. 
In the following methods, we make sure that on notifications we invoke the method just defined. 

```
MgdRawCardElement >> initializeAnnouncements

	card announcer
		when: MgdCardDisappearAnnouncement
		send: #disappear
		to: self.
	card announcer
		when: MgdCardFlipBackAnnouncement
		send: #drawBackSide
		to: self
```
```
MgdRawCardElement >> card: aMgdCard

	card := aMgdCard.
	self initializeAnnouncements
```

### Handling disappear


There are two ways to implement the disappearance of a card:
Either setting the opacity of the element to 0
(Note that the element is still present and receives events.)

```
MgdRawCardElement >> disappear
	self opacity: 0
```

Or changing the visibility as follows:

```
MgdRawCardElement >> disappear
	self visibility: BlVisibility hidden
```

	
Note that in the latter case, the element no longer receives events.
It is used for layout. 

![Selecting two cards that are not in pair.](figures/BoardMissedPair.png width=50&label=figBoardMissedPair)
### Refreshing on missed pair


When the player selects two cards that are not a pair, we present the two cards as shown in Figure *@figBoardMissedPair@*.
Now clicking on another card will flip back the previous cards. 

Remember, a card will raise a notification when flipped in either direction. 

```
MgdCardModel >> flip
	flipped := flipped not.
	self notifyFlipped
```


In the method `#resetStep` we see that all the previous cards are flipped \(toggled\).

```
MgdGameModel >> resetStep
	| lastCard |

	lastCard := self chosenCards  last.

	self chosenCards 
		allButLastDo: [ :aCard | aCard flip ];
		removeAll;
		add: lastCard
```


![Selecting two cards that are not a pair.](figures/BoardMissedPair.png width=60&label=figBoardMissedPair)


### Conclusion


At this stage, you are done for the simple interaction. Future versions of this document will explain how to add animations.


% !!! Main interaction done	
% At this stage you are done for the simple interaction. The following chapter will explain how we can add animation. 


% !! Adding animation
