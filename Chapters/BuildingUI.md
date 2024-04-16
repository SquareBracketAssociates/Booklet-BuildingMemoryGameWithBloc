## Basic building card graphical elements

In this chapter, we will build the visual appearance of the cards step by step.
In Bloc, visual objects are called elements, which are usually subclasses of `BlElement`, the inheritance tree root. In subsequent chapters, we will add interaction using event listeners.



### First: the card element

Our graphic element representing a card will be a subclass of the `BlElement` which has a reference to a card model.

```
BlElement << #MGCardElement
	slots: { #card };
	package: 'Bloc-Memory'
```


The message `backgroundPaint` will be used later to customize the background of our card element.
Let us define a nice color. 

```
MGCardElement >> backgroundPaint
	^ Color lightGray
```


We mentioned the accessors since the setter will be a place to hook registration for the communication between the model and the view.

```
MGCardElement >> card
	^ card
```


```
MGCardElement >> card: aMgCard
	card := aMgCard
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


![A first extremely basic representation of face down card.](figures/Inspecting0.png width=60&label=figInspecting0)

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
backgroundPaint	"Return a BlPaint that should be used as a background (fill)	of both back and face sides of the card. Color is polymorphic	with BlPaint and therefore can be used too."		^ Color pink darker
```



## Revisiting the look 

To display the different faces of a visual element we will 

 consists in placing other blElements as children of the one representing for example a card. 
This approach means that dynamically we will add and remove blElements. 



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

### Adding a cross


Now we are ready to define the backside of our card. We will start by drawing a line. To draw a line we should use the `BlLineGeometry`. At the end, we will create two lines and therefore two elements with a line geometry that will be added as children of the card Element.

Bloc uses parent-child relations between its elements thus leaving us with trees of elements where each node is an element, connected to a single parent and with zero to many children

A line is obviously defined between two points, we then need to give two points as parameters of the `from:to:` message from the `BlLineGeometry` class. 
Lines created using BlLineGeometry are a bit special as considered as "open geometries" meaning we don't define their color with the usual `background:` message like any other `BlElement`. Instead, we define a border for our line and give this border the color we wanted (here we chose light green), we also define the thickness of our line with the border's width.
Another particularity of open geometries is that they don't fit well with default outskirts in the current version of Bloc, this is why we redefine them to be centered 

```
MGCardElement >> initializeFirstLine

	| line |
	line := BlElement new
		        border: (BlBorder paint: Color lightGreen width: 3);
			geometry: BlLineGeometry new;
		        outskirts: BlOutskirts centered.
	line
		when: BlElementLayoutComputedEvent
		do: [ :e | line geometry from: 0 @ 0 to: line parent extent ].
	^ line
```
The message `when:do:` is used here to wait for the line parent to be drawn for the line to be defined, otherwise the `line parent extent` will be 0@0 and our line will not be displayed. 

We can redefine `drawBackSide` and add the line we just created.

```
MGCardElement >> drawBackSide

	self addChild: self initializeFirstLine.
	^ self
```

Once this method is defined, refresh the inspector and you should get a card as in Figure *@figOneLine@*.

![A rounded card with half of the cross.](figures/CardOneLine.png width=60&label=figOneLine)

### Full cross


Now we can add the second line to build a full cross. We will add another instance variable holding our back side so that the lines are created only once during initialization. Our solution is defined as follows: 

```
BlElement << #MGCardElement
	slots: { #card #backSide };
	package: 'Bloc-Memory'
```
```
MGCardElement >> backSide: aBlElement
	backside := aBlElement
```

Before creating the getter of backside, we will create the method that will be responsible for adding the two lines together as a cross `initializeBackSide`. Let's start by creating the second line the same way we created the first one.


```
MGCardElement >> initializeSecondLine

	| line |
	line := BlElement new
		        border: (BlBorder paint: Color lightGreen width: 3);
			geometry: BlLineGeometry new;
		        outskirts: BlOutskirts centered.
	line when: BlElementLayoutComputedEvent do: [ :e |
		line geometry from: 0 @ line parent height to: line parent width @ 0 ].
	^ line
```
```
MGCardElement >> initializeBackSide

	| firstLine secondLine cross |
	firstLine := self initializeFirstLine.
	secondLine := self initializeSecondLine.

	cross := BlElement new
		         addChildren: {
				         firstLine.
				         secondLine };
		         constraintsDo: [ :c |
			         c horizontal matchParent.
			         c vertical matchParent ].

	^ cross
```

Our backside is then an Element holding both lines, we tell this element to match its parent using constraints, meaning the element size will scale according to the parent size, this also makes our lines defined to the correct points. 

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


![A card with a complete backside.](figures/CardCross.png width=60&label=figCardCross)

Now our backside is fully implemented and when you refresh your view, you should get the card 
as shown in Figure *@figCardCross@*. 


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
MgdGameElement >> initialize
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
