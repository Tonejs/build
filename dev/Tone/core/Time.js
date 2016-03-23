define(["Tone/core/Tone"], function (Tone) {

	/**
	 *  @class Tone.Time is a flexible encoding of time
	 *         which can be evaluated to and from a string.
	 *         Parsing code modified from https://code.google.com/p/tapdigit/
	 *         Copyright 2011 2012 Ariya Hidayat, New BSD License
	 *  @extends {Tone}
	 *  @param  {Time}  val    The time value as a number or string
	 *  @param  {String=}  units  Unit values
	 *  @example
	 * Tone.Time(4, "n")
	 */
	Tone.Time = function(val, units){

		//allows it to be constructed with or without 'new'
		if (this instanceof Tone.Time) {

			/**
			 *  Any expressions parsed from the Time
			 *  @type  {Array}
			 *  @private
			 */
			this._expressions = [];

			/**
			 *  The units of time
			 *  @type  {String}
			 *  @private
			 */
			this._units = units;

			/**
			 *  The value of the time
			 *  @type  {Number}
			 *  @private
			 */
			this._value = this.defaultArg(val, Tone.Time.Type.Now);

			//get the value from the given time
			if (!this.isUndef(units)){
				// this._parseExpr(val);
			}

		} else {

			return new Tone.Time(val, units);
		}
	};

	Tone.extend(Tone.Time);

	/**
	 *  @enum {String}
	 *  @type  {Object}
	 *  @private
	 */
	Tone.Time.Type = {
		Divide : "/",
		Subtract : "-",
		Add : "+",
		Multiply : "*",
		Quantize : "@",
		Now : "now",
		Negative : "-"
	};

	/**
	 *  @enum {String}
	 *  @type  {Object}
	 *  @private
	 */
	Tone.Time.Units = {
		Measure : "m",
		Duplet : "n",
		Triplet : "t",
		TransportTime : "tr",
		Seconds : "s",
		Tick : "i",
		Hertz : "hz",
	};

	Tone.Time.prototype._evalValue = function(){
		switch(this._units){
			case Tone.Time.Units.Seconds:
				return this._value;
			case Tone.Time.Units.Measure:
				return this._timeSignature() * beatTime * this._value[0];
			case Tone.Time.Units.Duplet:
				return this._beatInSeconds() * this._value;
			case Tone.Time.Units.Triplet:
				return this._beatInSeconds() * this._value * (2/3);
			case Tone.Time.Units.Tick:
				return this._value * this._tickInSeconds();
			case Tone.Time.Units.Hertz:
				return 1 / this._value;
			case Tone.Time.Units.TransportTime:
				var beatTime = this._beatInSeconds();
				var bars = this._timeSignature() * beatTime * this._value[0];
				var beats = beatTime * this._value[1];
				var sixteenths = (beatTime * this._value[2]) / 4;
				return bars + beats + sixteenths;
		}
		return 0;
	};

	/*
	 *  the Expressions that Tone.Time can parse.
	 *
	 *  each expression belongs to a group and contains a regexp 
	 *  for selecting the operator as well as that operators method
	 *  
	 *  @type {Object}
	 *  @private
	 */
	Tone.Time._Expressions = {
		//values
		"value" : {
			"n" : {
				regexp : /^\d+n/i,
				name : Tone.Time.Units.Duplet
			},
			"t" : {
				regexp : /^\d+t/i,
				name : Tone.Time.Units.Triplet
			},
			"m" : {
				regexp : /^\d+m/i,
				name : Tone.Time.Units.Measure
			},
			"i" : {
				regexp : /^\d+i/i,
				name : Tone.Time.Units.Tick
			},
			"hz" : {
				regexp : /^\d+hz/i,
				name : Tone.Time.Units.Hertz
			},
			"tr" : {
				regexp : /^(\d+(\.\d+)?\:){1,2}(\d+(\.\d+)?)?/,
				name : Tone.Time.Units.TransportTime
			},
			"s" : {
				regexp : /^\d/,
				name : Tone.Time.Units.Seconds
			},
		},
		//syntactic glue
		"glue" : {
			"(" : {
				regexp : /^\(/
			},
			")" : {
				regexp : /^\)/
			},
			"," : {
				regexp : /^,/
			}
		},
		//binary expressions
		"binary" : {
			"+" : {
				regexp : /^\+/,
				precedence : 1,
				name : Tone.Time.Type.Add
			},
			"-" : {
				regexp : /^\-/,
				precedence : 1,
				name : Tone.Time.Type.Subtract
			},
			"*" : {
				regexp : /^\*/,
				precedence : 0,
				name : Tone.Time.Type.Multiply
			},
			"/" : {
				regexp : /^\*/,
				precedence : 0,
				name : Tone.Time.Type.Divide
			},
			"@" : {
				regexp : /^@/,
				precedence : 0,
				name : Tone.Time.Type.Quantize
			}
		},
		//unary expressions
		"unary" : {
			"neg" : {
				regexp : /^\-/,
				name : Tone.Time.Type.Negative
			},
			"now" : {
				regexp : /^\+/,
				name : Tone.Time.Type.Now
			},
		},
	};

	/**
	 *  tokenize the expression based on the Expressions object
	 *  @param   {string} expr 
	 *  @return  {Object}      returns two methods on the tokenized list, next and peek
	 *  @private
	 */
	Tone.Time.prototype._tokenize = function(expr){
		var position = -1;
		var tokens = [];

		while(expr.length > 0){
			expr = expr.trim();
			var token = getNextToken(expr);
			tokens.push(token);
			expr = expr.substr(token.value.length);
		}

		function getNextToken(expr){
			for (var type in Tone.Time._Expressions){
				var group = Tone.Time._Expressions[type];
				for (var opName in group){
					var op = group[opName];
					var reg = op.regexp;
					var match = expr.match(reg);
					if (match !== null){
						return {
							group : type,
							name : op.name,
							precedence : op.precedence,
							value : match[0],
						};
					}
				}
			}
			throw new SyntaxError("Unexpected token "+expr);
		}

		return {
			next : function(){
				return tokens[++position];
			},
			peek : function(){
				return tokens[position + 1];
			}
		};
	};

	Tone.Time.prototype._parseExpression = function(lexer, precedence){
		if (this.isUndef(precedence)){
			precedence = 5;
		}
		var expr;
		if (precedence < 0){
			expr = this._parseUnary(lexer);
		} else {
			expr = this._parseExpression(lexer, precedence-1);
		}
		var token = lexer.peek();
		while (token && token.group === "binary" && token.precedence === precedence){
			token = lexer.next();
			expr = {
				type : token.name,
				args : [
					expr,
					this._parseExpression(lexer, precedence)
				]
			};
			token = lexer.peek();
		}
		return expr;
	};

	Tone.Time.prototype._parseUnary = function(lexer){
		var token, expr;
		token = lexer.peek();
		while (token && token.group === "unary"){
			token = lexer.next();
			expr = this._parseUnary(lexer);
			if (token.name === Tone.Time.Type.Negate){

			}
			return {
				operator: token.value,
				method : token.type,
				args : [expr]
			};
		}
		return this._parsePrimary(lexer);
	};

	Tone.Time.prototype._parsePrimary = function(lexer){
		var token, expr;
		token = lexer.peek();
		if (this.isUndef(token)) {
			throw new SyntaxError("Unexpected termination of expression");
		}
		if (token.group === "value") {
			token = lexer.next();
			var value = token.value;
			var units = token.name;
			if (units === Tone.Time.Units.Measure || 
				units === Tone.Time.Units.Duplet || 
				units === Tone.Time.Units.Triplet || 
				units === Tone.Time.Units.Tick){
				value = parseInt(token.value);
			} else if (units === Tone.Time.Units.Seconds || 
				units === Tone.Time.Units.Hertz){
				value = parseInt(token.value);
			} else if (units === Tone.Time.Units.TransportTime){
				var values = token.value.split(":");
				for (var i = 0; i < values.length; i++){
					values[i] = parseFloat(values[i]);
				}
				value = values;
			}
			return Tone.Time(value, units);
		}
		if (token && token.group === "glue" && token.value === "("){
			lexer.next();
			expr = this._parseExpression(lexer);
			token = lexer.next();
			if (!(token && token.group === "glue" && token.value === ")")) {
				throw new SyntaxError("Expected )");
			}
			return expr;
		}
		throw new SyntaxError("Parse error, cannot process token " + token.value);
	};


	Tone.Time.prototype._traverseTree = function(left, right, exprType){
		if (left instanceof Tone.Time && right instanceof Tone.Time){
			left._pushExpr(right, exprType);
			return left;
		} else {
			left = this._traverseTree(left.args[0], left.args[1], left.type);
			right = this._traverseTree(right.args[0], right.args[1], right.type);
			this._traverseTree(left, right);
		}
	};

	/**
	 *  recursively parse the string expression into a syntax tree
	 *  
	 *  @param   {string} expr 
	 *  @return  {Object}
	 *  @private
	 */
	Tone.Time.prototype._parseTree = function(exprString){
		var lexer = this._tokenize(exprString);
		var tree = this._parseExpression(lexer);
		debugger;
		var res = this._traverseTree(tree);
		console.log(res);
		return tree;
	};

	/**
	 *  recursively evaluate the expression tree
	 *  @param   {Object} tree 
	 *  @return  {AudioNode}      the resulting audio node from the expression
	 *  @private
	 */
	Tone.Time.prototype._eval = function(tree){
		if (!this.isUndef(tree)){
			var node = tree.method(tree.args, this);
			this._nodes.push(node);
			return node;
		} 
	};

	/**
	 *  Takes a time expression and returns the units
	 *  @param  {String|Number}  val  The expression to parse
	 *  @return  {Tone.Time.Units}  The units
	 *  @private
	 */
	Tone.Time.prototype._parseUnits = function(val){
		if (this.isNumber(val)){
			return Tone.Time.Units.Seconds;
		} else if (this.isString(val)){
			if (parseFloat(val) == val){
				return Tone.Time.Units.Seconds;
			} else if (RegExp(/^\d+n$/i).test(val)){
				return Tone.Time.Units.Duplet;
			} else if (RegExp(/^\d+t$/i).test(val)){
				return Tone.Time.Units.Triplet;
			} else if (RegExp(/^\d+m$/i).test(val)){
				return Tone.Time.Units.Measure;
			} else if (RegExp(/^\d+i$/i).test(val)){
				return Tone.Time.Units.Tick;
			} else if (RegExp(/^\d+hz$/i).test(val)){
				return Tone.Time.Units.Hertz;
			} else if (RegExp(/^\d+n$/i).test(val)){
				return Tone.Time.Units.Duplet;
			} else if (RegExp(/^(\d+(\.\d+)?\:){1,2}(\d+(\.\d+)?)?$/i).test(val)){
				return Tone.Time.Units.TransportTime;
			}
		} 
	};

	/**
	 *  If there's a transport return the time of a beat in seconds
	 *  @return  {Number}
	 *  @private
	 */
	Tone.Time.prototype._beatInSeconds = function(){
		if (Tone.Transport){
			return 60 / Tone.Transport.bpm.value;
		} else {
			return 0;
		}
	};

	/**
	 *  If there's a transport return the time of a tick in seconds
	 *  @return  {Number}
	 *  @private
	 */
	Tone.Time.prototype._tickInSeconds = function(){
		if (Tone.Transport){
			return this._beatInSeconds() / Tone.Transport.PPQ;
		} else {
			return 0;
		}
	};

	/**
	 *  If there's a transport return the time signature
	 *  @return  {Number}
	 *  @private
	 */
	Tone.Time.prototype._timeSignature = function(){
		if (Tone.Transport){
			return Tone.Transport.timeSignature;
		} else {
			return 0;
		}
	};

	/**
	 *  Evaluate this time objects own value
	 *  @return  {Number}
	 */
	Tone.Time.prototype._evalValue = function(){
		switch(this._units){
			case Tone.Time.Units.Seconds:
				return this._value;
			case Tone.Time.Units.Measure:
				return this._timeSignature() * beatTime * this._value[0];
			case Tone.Time.Units.Duplet:
				return this._beatInSeconds() * this._value;
			case Tone.Time.Units.Triplet:
				return this._beatInSeconds() * this._value * (2/3);
			case Tone.Time.Units.Tick:
				return this._value * this._tickInSeconds();
			case Tone.Time.Units.Hertz:
				return 1 / this._value;
			case Tone.Time.Units.TransportTime:
				var beatTime = this._beatInSeconds();
				var bars = this._timeSignature() * beatTime * this._value[0];
				var beats = beatTime * this._value[1];
				var sixteenths = (beatTime * this._value[2]) / 4;
				return bars + beats + sixteenths;
		}
		return 0;
	};

	/**
	 *  Push an expression onto the expression list
	 *  @param  {Time}  val
	 *  @param  {String}  type
	 *  @param  {String}  units
	 *  @return  {Tone.Time} 
	 *  @private
	 */
	Tone.Time.prototype._pushExpr = function(val, type, units){
		if (!(val instanceof Tone.Time)){
			val = new Tone.Time(val, units);
		}
		this._expressions.push({
			type : type,
			value : val
		});
		return this;
	};

	Tone.Time.prototype.add = function(val, units){
		return this._pushExpr(val, Tone.Time.Type.Add, units);
	};

	Tone.Time.prototype.sub = function(val, units){
		return this._pushExpr(val, Tone.Time.Type.Subtract, units);
	};

	Tone.Time.prototype.mult = function(val, units){
		return this._pushExpr(val, Tone.Time.Type.Multiply, units);
	};

	Tone.Time.prototype.div = function(val, units){
		return this._pushExpr(val, Tone.Time.Type.Divide, units);
	};

	Tone.Time.prototype.quantize = function(val, units){
		return this._pushExpr(val, Tone.Time.Type.Quantize, units);
	};

	Tone.Time.prototype.fromNow = function(){
		return this._pushExpr(0, Tone.Time.Type.Now);
	};

	Tone.Time.prototype.eval = function(now){
		var val = this._evalValue();
		for (var i = 0; i < this._expressions.length; i++){
			var expr = this._expressions[i];
			switch(expr.type){
				case Tone.Time.Type.Add :
					val += expr.value.eval();
					break;
				case Tone.Time.Type.Subtract :
					val -= expr.value.eval();
					break;
				case Tone.Time.Type.Multiply :
					val *= expr.value.eval();
					break;
				case Tone.Time.Type.Divide :
					val /= expr.value.eval();
					break;
				case Tone.Time.Type.Now :
					val += this.defaultArg(now, this.now());
					break;
				case Tone.Time.Type.Quantize :
					val /= expr.value.eval();
					break;
			}
		}
		return val;
	};

	return Tone.Time;
});