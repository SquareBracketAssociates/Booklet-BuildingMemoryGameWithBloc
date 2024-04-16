## Basic building card graphical elements

In this chapter, we will build the visual appearance of the cards step by step.
In Bloc, visual objects are called elements, which are usually subclasses of `BlElement`, the inheritance tree root. In subsequent chapters, we will add interaction using event listeners.



### First: the card element

Our graphic element representing a card will be a subclass of the `BlElement` which has a reference to a card model.

```
BlElement << #MGCardElement
	slots: { #card };
	tag: 'Elements';
	package: 'Bloc-Memory'
```



We define the corresponding accessors since the setter methods will be the place to hook registration for the communication 
between the model and the view, as we will show later.

```
MGCardElement >> card
	^ card
```

```
MGCardElement >> card: aMgCard
	card := aMgCard
```


The message `backgroundPaint` will be used later to customize the background of our card element.
Let us define a nice color. 

```
MGCardElement >> backgroundPaint
	"Return a BlPaint that should be used as a background (fill)
	of both back and face sides of the card. Colors are polymorphic
	with BlPaint and therefore can be used too."
	
	^ Color pink darker
```


We define a method `initialize` to set the size and the default color as well as a card model object.

```
MGCardElement >> initialize
	super initialize.
	self size: 80 @ 80.
	self background: self backgroundPaint.
	self card: (MGCard new symbol: $a)
```


### Starting to draw a card

In Bloc, BlElements draw themselves onto the integrated canvas of the inspector as we inspect them, take a look at our element by executing this:

```
MGCardElement new
```


![A first extremely basic representation of face down card.](figures/Inspecting1.png width=60&label=figInspecting1)

### Improving the card visual 


Instead of displaying a full rectangle, we want a better visual. 
Bloc lets us decide the geometry we want to give to our elements, it could be a circle, a triangle or a rounded rectangle for example, you can check available geometries by looking at subclasses of `BlElementGeometry`.
We can also add a png as we will show later.

We can start giving a circle shape to our element, we will need to use the `geometry:` message and give a `BlCircleGeometry` as a parameter.

```
MGCardElement >> initialize

	super initialize.
	self size: 80 @ 80.
	self background: self backgroundPaint.
	self geometry: BlCircleGeometry new.
	self card: (MGCard new symbol: $a)
```


![A card with circular background.](figures/CardCircle.png width=60&label=figCardCircle)

However, we don't want the card to be a circle either. Ideally, it should be a rounded rectangle. So let's first add a helper method that would provide us with a corner radius:


```
MGCardElement >> cornerRadius
	^ 12
```


We would like to have a rounded rectangle so we use the `BlRoundedRectangleGeometry` class. However, we need to give the corner radius we just defined as a parameter of the `cornerRadius:` class message:

```
MGCardElement >> initialize

	super initialize.
	self size: 80 @ 80.
	self background: self backgroundPaint.
	self geometry: (BlRoundedRectangleGeometry cornerRadius: self cornerRadius).
	self card: (MgdCardModel new symbol: $a)
```


You should get then a visual representation close to the one shown in Figure *@figrounded@*.


![A rounded card.](figures/CardRounded.png width=60&label=figrounded)


We can change the color of the background by changing the definition of the method `backgroundPaint`

```

```




```
BlElement << #MGCardElement
	slots: { #card . #backSide . #flippedSide };
	package: 'Bloc-Memory'
```
```
MGCardElement >> flippedSide

	^ flippedSide ifNil: [ self initializeFlippedSide ]
```

We can now create the method that will create the text for the flipped side, this method will be called during initialization.

```
MGCardElement >> initializeFlippedSide

	| elt |
	elt := BlTextElement new text: self card symbol asRopedText.
	elt text fontName: 'Source Sans Pro'.
	elt text fontSize: 50.
	elt text foreground: Color white.
	^ elt
```

Now we can redefine `drawFlippedSide` to add our text element as a child of our card element

```
MGCardElement >> drawFlippedSide

	self removeChildren.
	self addChild: self flippedSide
```


When we refresh the display we can see the letter 'a' appear but it is positioned in the top left corner of our element, just as shown in Figure *@figCardNotCentered@*

![Not centered letter.](figures/CardNotCentered.png width=60&label=figCardNotCentered)

Let's change that!
We will have to use constraints on our text element to tell him to get aligned in the center of its parent. To achieve this goal, we need to define a layout on the parent which is our card Element. We will use a Frame Layout that acts just like a real frame, meaning the text element will be able to get centered into the frame of its parent. Let's add the frame layout in the initialization of the card Element.

```
MGCardElement >> initialize

	super initialize.
	self size: 80 @ 80.
	self background: self backgroundPaint.
	self geometry:
		(BlRoundedRectangleGeometry cornerRadius: self cornerRadius).
	self card: (MGCardModel new symbol: $a).
	self drawCardElement.
	self layout: BlFrameLayout new.
```

We can now add the constraints to the text element.

```
MGCardElement >> initializeFlippedSide

	| elt |
	elt := BlTextElement new text: self card symbol asRopedText.
	elt text fontName: 'Source Sans Pro'.
	elt text fontSize: 50.
	elt text foreground: Color white.
	elt constraintsDo: [ :c |
		c frame horizontal alignCenter.
		c frame vertical alignCenter ].
	^ elt
```


With this definition, we get a centered letter as shown in Figure *@figCardCentered@*.

![Centered letter.](figures/CardCentered.png width=60&label=figCardCentered)


### Flipped side 

Now we are ready to develop the flipped side of the card. To see if we should change the card model you can use the inspector to get the card element and send it the message `card flip` or directly 
recreate a new card  as follows: 

```
| cardElement | 
cardElement := MGCardElement new.
cardElement card flip.
cardElement
```


You should get an inspector in the situation shown in Figure *@figCardForFlip@*.
Now we are ready to implement the flipped side. 

![A flipped card without any visuals.](figures/CardForFlip.png width=60&label=figCardForFlip)

Let us redefine `drawFlippedSide` as follows: 
- First, we create a text element that holds the symbol of the card, we also give properties to this text by changing the font of the text but also its size and its color.
- Then we add the text element as a child of our card element

We will add an instance variable 'flippedSide' to our `MGCardElement` class so that we create the text only once during the initialization. We don't forget about getter and setter.



Now we are ready to work on the board game.

## Adding a board view

In the previous chapter, we defined all the card visualization. We are now ready to define the game board visualization.
Basically, we will define a new element subclass and set its layout.


Here is a typical scenario to create the game: we create a model and its view and we assign the model as the view's model.

```
game := MGGame numbers.
grid := MGGameElement new.
grid memoryGame: game. 
```


### The GameElement class

Let us define the class `MGGameElement` that will represent the game board. 
As for the `MGCardElement`, it inherits from the `BlElement` class. 
This view object holds a reference to the game model.
```
BlElement << #MGGameElement
	slots: { #memoryGame };
	package: 'Bloc-MemoryGame'
```


We define the `memoryGame:` setter method. We will extend it to create
all the card elements shortly. 

```
MGGameElement >> memoryGame: aMgdGameModel
	memoryGame := aMgdGameModel
```


```
MGGameElement >> memoryGame
	^ memoryGame
```


During the object initialization, we set the layout \(i.e., how sub-elements are placed inside their container\).
Here we define the layout to be a grid layout and we set it as horizontal.

```
MGGameElement >> initialize
	super initialize.
 	self background: Color veryLightGray.
	self layout: BlGridLayout horizontal.
```


### Creating cards


When a model is set for a board game, we use the model information to perform the following actions: 
- we set the number of columns of the layout
- we create all the card elements paying attention to set their respective model. 

Note in particular that we add all the card graphical elements as children of the board game using the message `addChild:`.

```
MgdGameElement >> memoryGame: aGameModel
	memoryGame := aGameModel.
	memoryGame availableCards
		do: [ :aCard | self addChild: (MGCardElement card: aCard) ]
```


```
MGCardElement class >> card: aCard 
	^ self new card: aCard
```



![A first board - not really working.](figures/BoardStarted.png width=60&label=figBoardStarted)

When we refresh the inspector we obtain a situation similar to the one of Figure *@figBoardStarted@*.
It shows that only a small part of the game is displayed. This is due to the fact that the game element 
did not adapt to its children. 


### Updating the container to its children


A layout is responsible for the layout of the children of a container but not of the container itself. 
For this, we should use constraints. 

```
MGGameElement >> initialize
	super initialize.
	self layout: BlGridLayout horizontal.
	self
		constraintsDo: [ :aLayoutConstrants | 
			aLayoutConstraints horizontal fitContent.
			aLayoutConstraints vertical fitContent ]
```


Now when we refresh our view we should get a situation close to the one presented in Figure*@figBoardOneRow@*, i.e., having 
just one row. Indeed we never mentioned to the layout that it should layout its children into a grid, wrapping after four.

![Displaying a row.](figures/BoardOneRow.png width=60&label=figBoardOneRow)


### Getting all the children displayed


We modify the `memoryGame:` method to set the number of columns 
that the layout should handle. 

```
MGGameElement >> memoryGame: aGameModel
	memoryGame := aGameModel.
	self layout columnCount: memoryGame gridSize.
	memoryGame availableCards
		do: [ :aCard | self addChild: (self newCardElement card: aCard) ]
```


Once the layout is set with the correct information we obtain a full board as shown in Figure *@figBoardFull@*.

![Displaying a full board.](figures/BoardFull.png width=60&label=figBoardFull)


### Separating cards


To offer a better identification of the cards, we should add some space between each of them. 
We achieve this by using the message `cellSpacing:` as shown below. 

We take the opportunity to change the background color using the message `background:`.
Note that a background is not necessarily a color but that color is polymorphic to a background
, therefore, the expression `background: Color gray darker` is equivalent to `background: (BlBackground paint: Color gray darker)`.

```
MGGameElement >> initialize
	super initialize.
	self background: (BlBackground paint: Color gray darker).
	self layout: (BlGridLayout horizontal cellSpacing: 20).
	self
		constraintsDo: [ :aLayoutConstraints | 
			aLayoutConstraints horizontal fitContent.
			aLayoutConstraints vertical fitContent ]
```


Once this method is changed, you should get a situation similar to the one described by Figure *@figBoardFullSpace@*.
![Displaying a full board with space.](figures/BoardFullSpace.png width=60&label=figBoardFullSpace)

Before adding interaction let's define a method `openWith:` that will open our game element with a given model.

```
MGGameElement class >> openWith: aMGGame

	| space gameElement |
	space := BlSpace new.
	gameElement := self new memoryGame: aMGGame.
	space root addChild: gameElement.
 
	space show
```

If we try to open this, we see our game element with all its cards but there's still some blank space around it, we can deal with this by changing the size of the space we put our game element into.

```
MGGameElement class >> openWith: aMGGame

	| space gameElement |
	space := BlSpace new.
	gameElement := self new memoryGame: aMGGame.
	space root addChild: gameElement.
 	space pulse.
  	space extent: gameElement extent.
   
	space show
```
Notice we send `pulse` to our space before changing its extent, it is required to tell the space to be prepared to change.

We are now ready to add interaction to the game. 



## Revisiting the look 

In the previous chapters, we saw that we can use a png to draw the back of cards. 
In this chapter, we propose a variation using other elements. 
This approach consists in placing other blElements as children of the one representing for example a card. 
This approach means that dynamically we will add and remove blElements. 

You can implement this either in the class MGCardElement or as a subclass.

### Preparing flipping


We define now two methods 

```
MGCardElement >> drawBackside
	"nothing for now"
```


```
MGCardElement >> drawFlippedSide
	"nothing for now"
```


We can now define the method that will draw our card 

```
MGCardElement >> drawCardElement

	self card isFlipped
		  ifTrue: [ self drawFlippedSide ]
		  ifFalse: [ self drawBackSide ]
```


Now we are ready to implement the backside and flipped side

 We will add another instance variable holding our back side so that the lines are created only once during initialization. Our solution is defined as follows: 

```
BlElement << #MGCardElement
	slots: { #card . #backSide };
	package: 'Bloc-Memory'
```
```
MGCardElement >> backSide: aBlElement
	backside := aBlElement
```

Before creating the getter of backside, we will create the method that will be responsible for adding the two lines together as a cross `initializeBackSide`. 
We can now define the `backSide` getter but with a little twist, using lazy initialization. This will create our element only when accessed the first time and not right after the initialization of the card element. This concept is very useful in certain situations and is great to know in case you need it, we define it as such :


```
MGCardElement >> backSide
	^ backSide ifNil: [ self initializeBackSide ]
```

We can finally redefine `drawBackSide` and call it in our initialization to draw our backside when the card is created. 

```
MGCardElement >> drawBackSide
	self removeChildren.
	self addChild: self backSide
```

```
MGCardElement >> initialize

	super initialize.
	self size: 80 @ 80.
	self background: self backgroundPaint.
	self geometry:
		(BlRoundedRectangleGeometry cornerRadius: self cornerRadius).
	self card: (MGCard new symbol: $a).
	self drawCardElement.
```


![UPDATE A card with a complete backside.](figures/CardCross.png width=60&label=figCardCross)

Now our backside is fully implemented and when you refresh your view, you should get the card 
as shown in Figure *@figCardCross@*. 


### Resources

If you want to use your own pngs, have a look at the class `ReaderWriterPNG` that converts PNG files into Forms. A form is a piece of graphical memory.

If you want to manage forms as the method cardbackForm provided in the project, you can have a look 
at the IconFactory project on github. 

```
Metacello new
    baseline: #IconFactory;
    repository: 'github://pharo-graphics/IconFactory';
    load
``` 

This project supports the definition of form as textual resources in methods that can be then versioned altogether with the code. 

Given a base64 encoded string you can get a form with the following expression, here we take the base64 encoded string from `IconFactoryTest new exampleIconContents`

```
Form fromBinaryStream: IconFactoryTest new exampleIconContents base64Decoded asByteArray readStream
```

Following this you can generate a method body with a cache (here named icons) as follows: 

```
iconMethodTemplate	^ '{1}	"Private - Generated method"	^ self icons		at: #{1}		ifAbsentPut: [ Form fromBinaryStream: IconFactoryTest new exampleIconContents
					 base64Decoded asByteArray readStream ]'
```
Where the first argument is part of a method name for example 'tintin'.

### Conclusion



