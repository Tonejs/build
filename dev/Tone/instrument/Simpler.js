define(["Tone/core/Tone", "Tone/source/Player", "Tone/component/AmplitudeEnvelope", "Tone/component/FrequencyEnvelope",
	"Tone/component/Filter", "Tone/instrument/Instrument"], 
function(Tone){

	"use strict";

	/**
	 *  @class A sampler instrument which plays an audio buffer 
	 *         through an amplitude envelope and a filter envelope. The sampler takes
	 *         an Object in the constructor which maps a sample name to the URL 
	 *         of the sample. Nested Objects will be flattened and can be accessed using
	 *         a dot notation (see the example).
	 *         <img src="https://docs.google.com/drawings/d/1UK-gi_hxzKDz9Dh4ByyOptuagMOQxv52WxN12HwvtW8/pub?w=931&h=241">
	 *
	 *  @constructor
	 *  @extends {Tone.Instrument}
	 *  @param {Object|string} urls the urls of the audio file
	 *  @param {Object} [options] the options object for the synth
	 *  @example
	 * var sampler = new Simpler({
	 * 	A : {
	 * 		1 : "./audio/casio/A1.mp3",
	 * 		2 : "./audio/casio/A2.mp3",
	 * 	},
	 * 	"B.1" : "./audio/casio/B1.mp3",
	 * }).toMaster();
	 * 
	 * //listen for when all the samples have loaded
	 * Tone.Buffer.onload = function(){
	 * 	sampler.triggerAttack("A.1", time, velocity);
	 * };
	 */
	Tone.Simpler = function(urls, options){

		options = this.defaultArg(options, Tone.Simpler.defaults);
		Tone.Instrument.call(this, options);

		/**
		 *  The sample player.
		 *  @type {Tone.Player}
		 */
		this.player = new Tone.Player(options.player);
		this.player.retrigger = true;

		/**
		 *  the buffers
		 *  @type {Object}
		 *  @private
		 */
		this._buffers = {};

		/**
		 *  The amplitude envelope. 
		 *  @type {Tone.AmplitudeEnvelope}
		 */
		this.envelope = new Tone.AmplitudeEnvelope(options.envelope);

		/**
		 *  The name of the current sample. 
		 *  @type {string}
		 *  @private
		 */
		this._sample = options.sample;

		/**
		 * the private reference to the pitch
		 * @type {number}
		 * @private
		 */
		this._pitch = options.pitch;

		//connections / setup
		this._loadBuffers(urls);
		this.pitch = options.pitch;
		this.player.chain(this.envelope, this.output);
		this._readOnly(["player", "envelope", "filter"]);
	};

	Tone.extend(Tone.Simpler, Tone.Instrument);

	/**
	 *  the default parameters
	 *  @static
	 */
	Tone.Simpler.defaults = {
		"sample" : 0,
		"pitch" : 0,
		"player" : {
			"loop" : false,
		},
		"envelope" : {
			"attack" : 0.001,
			"decay" : 0,
			"sustain" : 1,
			"release" : 0.1
		}
	};

	/**
	 *  load the buffers
	 *  @param   {Object} urls   the urls
	 *  @private
	 */
	Tone.Simpler.prototype._loadBuffers = function(urls){
		if (this.isString(urls)){
			this._buffers["0"] = new Tone.Buffer(urls, function(){
				this.sample = "0";
			}.bind(this));
		} else {
			urls = this._flattenUrls(urls);
			for (var buffName in urls){
				this._sample = buffName;
				var urlString = urls[buffName];
				this._buffers[buffName] = new Tone.Buffer(urlString);
			}
		}
	};

	/**
	 *  Flatten an object into a single depth object. 
	 *  thanks to https://gist.github.com/penguinboy/762197
	 *  @param   {Object} ob 	
	 *  @return  {Object}    
	 *  @private
	 */
	Tone.Simpler.prototype._flattenUrls = function(ob) {
		var toReturn = {};
		for (var i in ob) {
			if (!ob.hasOwnProperty(i)) continue;
			if (this.isObject(ob[i])) {
				var flatObject = this._flattenUrls(ob[i]);
				for (var x in flatObject) {
					if (!flatObject.hasOwnProperty(x)) continue;
					toReturn[i + "." + x] = flatObject[x];
				}
			} else {
				toReturn[i] = ob[i];
			}
		}
		return toReturn;
	};

	/**
	 *  Start the sample and simultaneously trigger the envelopes. 
	 *  @param {string=} sample The name of the sample to trigger, defaults to
	 *                          the last sample used. 
	 *  @param {Time} [time=now] The time when the sample should start
	 *  @param {number} [velocity=1] The velocity of the note
	 *  @returns {Tone.Simpler} this
	 *  @example
	 * sampler.triggerAttack("B.1");
	 */
	Tone.Simpler.prototype.triggerAttack = function(name, time, velocity){
		time = this.toSeconds(time);
		if (name){
			this.sample = name;
		}
		this.player.start(time);
		this.envelope.triggerAttack(time, velocity);
		return this;
	};

	/**
	 *  Start the release portion of the sample. Will stop the sample once the 
	 *  envelope has fully released. 
	 *  
	 *  @param {Time} [time=now] The time when the note should release
	 *  @returns {Tone.Simpler} this
	 *  @example
	 * sampler.triggerRelease();
	 */
	Tone.Simpler.prototype.triggerRelease = function(time){
		time = this.toSeconds(time);
		this.envelope.triggerRelease(time);
		this.player.stop(this.toSeconds(this.envelope.release) + time);
		return this;
	};

	/**
	 * The name of the sample to trigger.
	 * @memberOf Tone.Simpler#
	 * @type {number|string}
	 * @name sample
	 * @example
	 * //set the sample to "A.2" for next time the sample is triggered
	 * sampler.sample = "A.2";
	 */
	Object.defineProperty(Tone.Simpler.prototype, "sample", {
		get : function(){
			return this._sample;
		},
		set : function(name){
			if (this._buffers.hasOwnProperty(name)){
				this._sample = name;
				this.player.buffer = this._buffers[name];
			} else {
				throw new Error("Simpler does not have a sample named "+name);
			}
		}
	});

	/**
	 * The direction the buffer should play in
	 * @memberOf Tone.Simpler#
	 * @type {boolean}
	 * @name reverse
	 */
	Object.defineProperty(Tone.Simpler.prototype, "reverse", {
		get : function(){
			for (var i in this._buffers){
				return this._buffers[i].reverse;
			}
		}, 
		set : function(rev){
			for (var i in this._buffers){
				this._buffers[i].reverse = rev;
			}
		}
	});

	/**
	 * Repitch the sampled note by some interval (measured
	 * in semi-tones). 
	 * @memberOf Tone.Simpler#
	 * @type {Interval}
	 * @name pitch
	 * @example
	 * sampler.pitch = -12; //down one octave
	 * sampler.pitch = 7; //up a fifth
	 */
	Object.defineProperty(Tone.Simpler.prototype, "pitch", {
		get : function(){
			return this._pitch;
		},
		set : function(interval){
			this._pitch = interval;
			this.player.playbackRate = this.intervalToFrequencyRatio(interval);
		}
	});

	/**
	 *  Clean up.
	 *  @returns {Tone.Simpler} this
	 */
	Tone.Simpler.prototype.dispose = function(){
		Tone.Instrument.prototype.dispose.call(this);
		this._writable(["player", "filterEnvelope", "envelope", "filter"]);
		this.player.dispose();
		this.envelope.dispose();
		this.filter.dispose();
		this.player = null;
		this.envelope = null;
		this.filter = null;
		for (var sample in this._buffers){
			this._buffers[sample].dispose();
			this._buffers[sample] = null;
		}
		this._buffers = null;
		return this;
	};

	return Tone.Simpler;
});
