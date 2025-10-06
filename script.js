import * as THREE from "https://esm.sh/three";
import { WebGLRenderer, PerspectiveCamera, Scene, Vector3, Mesh, BoxGeometry } from "https://esm.sh/three";
import { EffectComposer } from 'https://esm.sh/three/examples/jsm/postprocessing/EffectComposer.js';
import { RenderPass } from 'https://esm.sh/three/examples/jsm/postprocessing/RenderPass.js';
import { ShaderPass } from 'https://esm.sh/three/examples/jsm/postprocessing/ShaderPass.js';
import { FXAAShader } from 'https://esm.sh/three/examples/jsm/shaders/FXAAShader.js';
import { Sky } from 'https://esm.sh/three/addons/objects/Sky.js';
import * as Tone from "https://esm.sh/tone";
import 'https://unpkg.com/fix-webm-duration@1.0.6/fix-webm-duration.js';

const InversionShader = {
  uniforms: {
    tDiffuse: { value: null },
    invertAmount: { value: 0.0 },
  },
  vertexShader: `
        varying vec2 vUv;
        
        void main() {
            vUv = uv;
            gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
        }
    `,
  fragmentShader: `
    uniform sampler2D tDiffuse;
    uniform float invertAmount;
    varying vec2 vUv;

    #define PI 3.14

    vec2 rot(vec2 v, float a) {
      return mat2(cos(a), -sin(a), sin(a), cos(a)) * v;
    }

    vec2 distortCoords1(vec2 uv, float rotMagnitude, float distMagnitude) {
      vec2 uv_ = uv;
      vec2 ruv = rot(uv_, -PI / rotMagnitude);
      float d = 1.0 - distance(ruv, vec2(0.5)) / distMagnitude;
      uv_ *= d;
      uv_ = clamp(uv_, 0.0, 1.0);
      return uv_;
    }

    void main() {
      vec2 uv = vUv;
      if (invertAmount > 0.) {
        float i = clamp(1.-invertAmount, 0.15, 1.);
        uv = distortCoords1(uv, uv.x+PI*i, cos(uv.y+uv.x)+2.+10.0*i);
      }
      if (invertAmount < 0.) {
        float i = clamp(1.-abs(invertAmount), 0.15, 1.);
        uv = distortCoords1(uv, uv.x-PI*i, sin(uv.y+uv.x)+100.*i);
      }
      vec4 originalColor = texture2D(tDiffuse, uv);
      // cause it ignores opacity
      if (originalColor.w <= 0.0) {
        gl_FragColor = vec4(0.0);
      } else {
        vec3 fullyInvertedColor = 1.0 - originalColor.rgb;
        vec3 finalRgb = mix(originalColor.rgb, fullyInvertedColor, abs(invertAmount));
        gl_FragColor = vec4(finalRgb, originalColor.w);
      }
    }`,
};

class PencilShader {
    vertexShader = `
    varying vec2 vUv;
    varying vec3 vPos;

    void main() {
      vUv = uv;
      vPos = position;
      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }`;

    fragmentShader = `
    #ifdef GL_ES
    precision mediump float;
    #endif

    uniform float freq;
    uniform float thicc;

    varying vec2 vUv;
    varying vec3 vPos;

    float rand(vec2 co){
        float r = fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
        if (r >= .93) return 1.;
        return 0.;
    }

    void main() {
      vec2 st = vUv;
      vec3 color = vec3(1.);  

      if (fract(st.y*freq+sin(st.x*10.+st.y*200.*vPos.y)/20.) > thicc*pow((1.-distance(st.x, .5)), 2.)) {
            color = vec3(rand(st));
      }
      gl_FragColor = vec4(color,1.0);
    }`;

  constructor(freq = 90, thicc = 0.7) {
    this.side = THREE.DoubleSide;
    this.uniforms = {
      freq: { value: freq },
      thicc: { value: thicc },
    };
  }
}

class WebglEngine {
  needToRender = true;
  renderer = new WebGLRenderer({ antialias: true, alpha: true });
  // renderTarger = new THREE.WebGLRenderTarget(window.innerWidth, window.innerHeight, { samples: 8 });
  // composer = new EffectComposer(this.renderer, this.renderTarger);
  composer = new EffectComposer(this.renderer);
  camera = new PerspectiveCamera(75, 1, 0.1, 100000);
  scene = new Scene();
//   controls = new OrbitControls(this.camera, this.renderer.domElement);

  postprocessing() {
    this.renderer.setPixelRatio(0.5);
    this.composer.addPass(new RenderPass(this.scene, this.camera));
    this.customPass = new ShaderPass(InversionShader);
    this.composer.addPass(this.customPass);
    const fxaaPass = new ShaderPass(FXAAShader);
    const pixelRatio = this.renderer.getPixelRatio();
    fxaaPass.material.uniforms.resolution.value.x = 1 / (window.innerWidth * pixelRatio);
    fxaaPass.material.uniforms.resolution.value.y = 1 / (window.innerHeight * pixelRatio);
    this.composer.addPass(fxaaPass);
  }

  setUniforms = (value) => {
    this.customPass.uniforms.invertAmount.value = value;
    this.needToRender = true;
  };

  onWindowResize = () => {
    const onWindowResize_ = () => {
      const [w, h] = [window.innerWidth, window.innerHeight];
      this.camera.aspect = w / h;
      this.camera.updateProjectionMatrix();
      this.renderer.setSize(w, h);
      this.composer.setSize(w, h, false);
      this.needToRender = true;
    };
    onWindowResize_();
    window.addEventListener('resize', onWindowResize_, false);
  };

  render = () => {
    requestAnimationFrame(this.render);
    if (this.needToRender || this.vertigoActive > 0) {
      this.needToRender = false;
      if (this.vertigoActive > 0) this.vertigo();
      this.composer.render();
    }
  };

  constructor() {
    this.render();
    document.body.appendChild(this.renderer.domElement);
    this.postprocessing();
    // this.renderer.setClearColor(0x000000, 0);
    this.scene.background = new THREE.Color('#ffe8d1');
    this.renderer.autoClear = false;
    // this.controls.enableDamping = true;
    // this.controls.autoRotate = true;
    this.onWindowResize();
    this.sceneSetup();
  }

  moveSpeed = 0.004;
  // fovSpeed = 0.01;
  defaultFOV = this.camera.fov;
  isMovingForward = true;
  vertigoActive = 0;
  defaultFocalLength = this.camera.getFocalLength();
  focalLengthSpeed = 0.025;

  cameraReset() {
    this.isMovingForward = true;
    // this.camera.fov = this.defaultFOV;
    this.camera.setFocalLength(this.defaultFocalLength);
    this.camera.updateProjectionMatrix();
  }

  vertigoEnable(state) {
    this.vertigoActive += state;
    this.cameraReset();
    if (state < 0 && this.vertigoActive > 0) {
      this.randomPosAndLook(false);
    }
  }

  vertigo() {
    const direction = new THREE.Vector3();
    this.camera.getWorldDirection(direction);
    const cfl = this.camera.getFocalLength();
    if (cfl > 80 || cfl < 5) this.isMovingForward = !this.isMovingForward;
    if (this.isMovingForward) {
      const moveVector = direction.multiplyScalar(this.moveSpeed);
      this.camera.position.add(moveVector);
      this.camera.setFocalLength(cfl - this.focalLengthSpeed);
      // this.camera.fov += this.fovSpeed;
    } else {
      const moveVector = direction.multiplyScalar(-this.moveSpeed);
      this.camera.position.add(moveVector);
      this.camera.setFocalLength(cfl + this.focalLengthSpeed);
      // this.camera.fov -= this.fovSpeed;
    }

    // this.camera.lookAt(targetPoint);
    this.camera.updateProjectionMatrix();
  }

  setCameraPosAndLookAt(pos, lookAt = new Vector3()) {
    this.camera.position.copy(pos);
    this.camera.lookAt(lookAt);
    // this.controls.update();
    this.needToRender = true;
  }

  randomPosAndLook(inc = true) {
    this.setCameraPosAndLookAt(
      new Vector3().randomDirection().multiplyScalar(10),
      new Vector3().randomDirection().multiplyScalar(10),
    );
    if (inc) this.vertigoEnable(1);
  }

  sceneSetup() {
    const sky = new Sky();
    sky.scale.setScalar(1000);
    const phi = THREE.MathUtils.degToRad(90);
    const theta = THREE.MathUtils.degToRad(180);
    const sunPosition = new Vector3().setFromSphericalCoords(1, phi, theta);
    sky.material.uniforms.sunPosition.value = sunPosition;
    sky.material.uniforms.turbidity.value = 0.1;
    sky.material.uniforms.rayleigh.value = 0.1;
    sky.material.uniforms.mieCoefficient.value = 10.001;
    sky.material.uniforms.mieDirectionalG.value = 0.7;
    this.scene.add(sky);

    this.setCameraPosAndLookAt(new Vector3(0, 10, 0));
    this.scene.add(this.camera);
    function createBox(size, freq, thicc) {
      return new Mesh(
        new BoxGeometry(size, size, size),
        new THREE.ShaderMaterial(new PencilShader(freq, thicc)),
      );
    }
    const boxes = [
      { pos: new Vector3(0, 2, 4), distance: 20 },
      { pos: new Vector3(0, 2, -4), distance: 20 },
      { pos: new Vector3(10, 2, -4), distance: 8 },
      { pos: new Vector3(10, 2, 4), distance: 8 },
      { pos: new Vector3(20, 2, 0), distance: 20 },
      { pos: new Vector3(-20, 2, 0), distance: 20 },
      { pos: new Vector3(-10, 2, -4), distance: 8 },
      { pos: new Vector3(-10, 2, 4), distance: 8 },
      { pos: new Vector3(-6, 2, -12), distance: 10 },
      { pos: new Vector3(-6, 2, 12), distance: 10 },
      { pos: new Vector3(6, 2, -12), distance: 10 },
      { pos: new Vector3(6, 2, 12), distance: 10 },
    ];
    const freq = 900;
    const thicc = 0.95;
    for (let i = 0; i < 16; i++) {
      for (const { pos, distance } of boxes.values()) {
        const b = createBox(6, freq + distance * i * 2, thicc + i / 100);
        b.position.copy(pos.add(new Vector3(0, -i * distance, 0)));
        this.scene.add(b);
      }
    }
  }
}

/**
 * Clamps the given value between min and max.
 *
 * @param {number} value - The value to clamp.
 * @param {number} min - The min value.
 * @param {number} max - The max value.
 * @return {number} The clamped value.
 */
function clamp( value, min, max ) {

	return Math.max( min, Math.min( max, value ) );

}

/**
 * Returns a random integer from "https://esm.sh/<low, high>" interval.
 *
 * @param {number} low - The lower value boundary.
 * @param {number} high - The upper value boundary
 * @return {number} A random integer.
 */
function randInt( low, high ) {

	return low + Math.floor( Math.random() * ( high - low + 1 ) );

}

const NOTES = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];

const MODES = {
  major: [0, 2, 4, 5, 7, 9, 11],
  minor: [0, 2, 3, 5, 7, 8, 10],
  majorPentatonic: [0, 2, 4, 7, 9],
  minorPentatonic: [0, 3, 5, 7, 10],
  harmonicMajor: [0, 2, 4, 5, 7, 8, 11],
  harmonicMinor: [0, 2, 3, 5, 7, 8, 11],
  melodicMinor: [0, 2, 3, 5, 7, 9, 11],
  dorian: [0, 2, 3, 5, 7, 9, 10],
  phrygian: [0, 1, 3, 5, 7, 8, 10],
  lydian: [0, 2, 4, 6, 7, 9, 11],
  mixolydian: [0, 2, 4, 5, 7, 9, 10],
  locrian: [0, 1, 3, 5, 6, 8, 10],
  blues: [0, 3, 5, 6, 7, 10],
  'ionian+q4': [0, 2, 4, 6, 7, 9, 11],
  'dorian-q2': [0, 1, 3, 5, 7, 9, 10],
  'edo31-major': [0, 5, 10, 13, 18, 23, 28],
  'edo31-minor': [0, 5, 8, 13, 18, 21, 26],
  'edo31-free': Array.from({ length: 31 }, (_, i) => i),
  '12-tone-communism': Array.from({ length: 12 }, (_, i) => i),
  hirajoshi: [0, 2, 3, 7, 8],
  iwato: [0, 1, 5, 6, 10],
  kumoi: [0, 2, 3, 7, 9],
  aketa: [0, 3, 4, 7, 9],
  kokinjoshi: [0, 2, 5, 7, 9],
  insen: [0, 1, 5, 7, 10],
  persian: [0, 1, 4, 5, 8, 9, 11],
  gypsyMajor: [0, 2, 4, 6, 7, 10, 11],
  tritone: [0, 1, 4, 6, 7, 10],
  enigmatic: [0, 1, 4, 6, 8, 10, 11],
  mystic: [0, 2, 4, 7, 9, 11, 1],
  pelog: [0, 1, 3, 7, 8],
  flamenco: [0, 1, 4, 5, 7, 8, 11],
  oxtascale: [0, 1, 3, 4, 6, 7, 9, 10],
  '9-scale': [0, 2, 4, 5, 7, 9, 11, 13, 14],
  '10-scale': [0, 2, 3, 5, 7, 8, 10, 12, 14, 15],
  scriabin: [0, 2, 4, 7, 9, 14],
  vietnamese1: [0, 2, 4, 6, 9],
  vietnamese2: [0, 2, 5, 7, 9],
  chinese: [0, 4, 6, 7, 11],
  mongolian: [0, 2, 7, 9, 14],
  balinese: [0, 1, 4, 7, 8],
  egyptian: [0, 2, 5, 7, 10],
  hijaz: [0, 1, 4, 5, 7, 8, 11],
  nahawand: [0, 2, 3, 5, 7, 8, 11],
  rast: [0, 2, 4, 5, 7, 9, 10],
  bayati: [0, 1, 3, 5, 7, 8, 10],
  saba: [0, 1, 3, 6, 7, 8, 10],
  ajam: [0, 2, 4, 5, 7, 9, 11],
};

class Scale {
  constructor(key, modeName, octave, prepared = false, standardPitch = 440) {
    this.scale = this.selectScale(key, modeName, octave, standardPitch);
    if (prepared) this.preparePitches();
  }

  preparePitches() {
    this.scale.range.forEach((n) => {
      n.pitch += (n.pitch / 100) * 5 * (Math.random() * 2 - 1);
    });
  }

  static shiftOctave(pitch, octave) {
    const shift = Math.abs(octave);
    return octave > 0 ? pitch * 2 ** shift : pitch / 2 ** shift;
  }

  selectScale(key, modeName, octave, standardPitch) {
    if (/q4|q2/.test(modeName)) {
      return new QuateredMicrotonalScale(key, modeName, octave, standardPitch);
    }
    if (/edo31/i.test(modeName)) {
      return new Edo31Scale(key, modeName, octave, standardPitch);
    }
    return new StandardScale(key, modeName, octave, standardPitch);
  }
}

class StandardScale {
  constructor(key, modeName, octave, standardPitch = 440) {
    this.standardPitch = standardPitch;
    this.key = key;
    this.modeName = modeName;
    this.mode = MODES[modeName];
    this.octave = octave;
    this.notes = this.getNotes();
    this.range = this.getRange();
  }

  parseNote(note) {
    const [, noteName, octaveStr] = note.match(/([a-z]#?)(\d+)/i);
    const octave = Number.parseInt(octaveStr);
    return { noteName, octave };
  }

  calcBasePitch(noteName, octave, pitch) {
    const noteIndex = NOTES.indexOf(noteName);

    const octaveDifference = octave - 4;
    const noteDifference = noteIndex - 9; // A4 index

    const semitoneDifference = octaveDifference * 12 + noteDifference;
    const frequencyRatio = 2 ** (semitoneDifference / 12);

    return pitch * frequencyRatio;
  }

  getNotePitch(note, pitch = this.standardPitch) {
    const { noteName, octave } = this.parseNote(note);
    const basePitch = this.calcBasePitch(noteName, octave, pitch);
    return basePitch;
  }

  getRange() {
    return Array.from({ length: 9 }, (_, i) => this.notes.map((n) => `${n}${this.octave + i}`))
      .flat()
      .map((note) => ({ note, pitch: this.getNotePitch(note) }));
  }

  getNotes(key = this.key, mode = this.mode) {
    const keyIndex = NOTES.indexOf(key);
    const notes = mode
      .map((interval) => (keyIndex + interval) % 12)
      .sort((a, b) => a > b)
      .map((i) => NOTES[i]);
    return Array.from(new Set(notes));
  }

  randomNote(min = 0, max = this.range.length - 1) {
    return this.range[randInt(min, max)];
  }
}

class QuateredMicrotonalScale extends StandardScale {
  getMicrotonalPitch(noteIndexInScale, basePitch) {
    if (this.modeName === 'ionian+q4' && noteIndexInScale === 3) {
      return basePitch * 2 ** (1 / 24); // Raise by one quarter tone
    }
    if (this.modeName === 'dorian-q2' && noteIndexInScale === 1) {
      return basePitch / 2 ** (1 / 24); // Lower by one quarter tone
    }
    return basePitch;
  }

  getNotePitch(note, pitch = this.standardPitch) {
    const { noteName, octave } = this.parseNote(note);
    const noteIndexInScale = this.notes.indexOf(noteName);
    const basePitch = this.calcBasePitch(noteName, octave, pitch);
    return this.getMicrotonalPitch(noteIndexInScale, basePitch);
  }
}

class Edo31Scale extends StandardScale {
  getNotePitch(step, pitch = this.standardPitch) {
    const referenceStep = 31 * 4 + NOTES.indexOf('A');
    const stepsFromA4 = step - referenceStep;
    const frequencyRatio = 2 ** (stepsFromA4 / 31);
    return pitch * frequencyRatio;
  }

  getRange() {
    return Array.from({ length: 9 }, (_, i) =>
      this.notes.map((step) => ({
        note: `s${step}`,
        step: step + 31 * i,
        pitch: this.getNotePitch(step + 31 * (i + this.octave)),
      })),
    ).flat();
  }

  getNotes(key = this.key, mode = this.mode) {
    const keyIndex = NOTES.indexOf(key);
    const stepIndex = Number.parseInt(`${keyIndex}${this.octave}`) % 31;
    return mode.map((interval) => (stepIndex + interval) % 31).sort((a, b) => a > b);
  }
}

class Phrase {
  constructor({
    sequence,
    arpegioSequence = [],
    synthout = undefined,
    tempo = 120,
    type = 'triangle',
    volume = -1,
    times = Infinity,
    tick = undefined,
    panner = 0,
  }) {
    this.tempo = tempo;
    this.times = times;
    this.tick = tick;

    this.sequence = sequence;
    this.arpegioSequence = arpegioSequence;
    this.phrase = this.sequence;

    if (!synthout) {
      this.synth = new Tone.PolySynth(Tone.Synth, {
        oscillator: { type },
        volume,
      });

      this.panner = new Tone.Panner(panner);
      this.synth.connect(this.panner);
      this.panner.toDestination();
    } else {
      this.synth = synthout;
    }

    this.index = 0;
    this.noteIndex = 0;
    this.loopIndex = 0;

    this.loop = new Tone.Loop(this._onTick.bind(this), 0);
    this.setTempo(tempo);
    this.loop.iterations = this.times;
  }

  _onTick(time) {
    const note = this.phrase[this.noteIndex];
    this.synth.triggerAttackRelease(note, this.loop.interval, time);

    if (this.tick) this.tick(note, this.index, this.noteIndex, this.loopIndex);

    this.index++;
    this.noteIndex = this.index % this.phrase.length;
    this.loopIndex = this.index / this.phrase.length;
  }

  start(at = 0, arpegio = false) {
    this.phrase = arpegio ? this.arpegioSequence : this.sequence;
    this.index = 0;
    this.loop.start(at);
    return this;
  }

  stop() {
    this.loop.stop();
  }

  setTempo(tempo) {
    this.tempo = tempo;
    this.loop.interval = (tempo > 10 ? 10 : 1000) / this.tempo;
  }

  dispose() {
    if (this.panner) {
      this.panner.dispose();
      this.synth.dispose();
    }
    this.loop.dispose();
  }
}

// piano phase
// http://localhost:5173/?arpseqTempo=88.875&arpseqArpegiator=false&arpseqSequencer=true&arpseqOctaves=0&audio-only=false&key=A%23&mode=major&octave=4&pitch=440&prepared=false&synth=MonoSynth.Pianoetta&volume=0&volumeMix=0.5&gain=1&decay=0.1&delayTime=0&velocity=0.4&distortion=0&vibratoDepth=0&vibratoFreq=0&chorusDepth=0&chorusFreq=0&chorusDelayTime=0&tremoloFreq=0&tremoloDepth=0&lowpass=5000&compressorRatio=1&arpseqHoldKeys=%5B329.63%2C369.99%2C493.88%2C554.37%2C587.33%2C369.99%2C329.63%2C554.37%2C493.88%2C369.99%2C587.33%2C554.37%5D
//
// EDO31 C shaman
// s29 s10/s18  s29  s4  s12  s4_  s10/s28 s18  s29  s29  s12
// z     q/o     z    5   e   m      q/l    o    z    z    e
// http://localhost:5173/?arpseqTempo=75&arpseqArpegiator=false&arpseqSequencer=true&arpseqOctaves=0&audio-only=false&key=C&mode=edo31-free&octave=3&pitch=440&prepared=false&synth=MonoSynth.Pianoetta&volume=0&volumeMix=0.5&gain=1&decay=0.1&delayTime=0&velocity=0.4&distortion=0&vibratoDepth=0&vibratoFreq=0&chorusDepth=0&chorusFreq=0&chorusDelayTime=0&tremoloFreq=0&tremoloDepth=0&lowpass=5000&compressorRatio=1&arpseqHoldKeys=%5B344.06110614675833%2C224.97451583227902%2C344.06110614675833%2C196.7294852990844%2C235.2635336856348%2C393.45897059816883%2C336.4534114998035%2C269.041010824843%2C344.06110614675833%2C344.06110614675833%2C235.2635336856348%5D

class ArpSeq {
  tempo = 120;
  arpegio = true;
  octaves = 0;
  synthout = undefined;
  holdState = false;
  holdKeys = [];
  nameMap = {
    holdKeys: 'arpseqHoldKeys',
    arpegio: 'arpseqArpegiator',
    octaves: 'arpseqOctaves',
    tempo: 'arpseqTempo',
  };

  constructor(synthout, callback) {
    this.callback = callback;
    this.synthout = synthout;
  }

  export() {
    return Object.entries(this.nameMap).reduce(
      (acc, [k, v]) => Object.assign(acc, { [v]: this[k] }),
      {},
    );
  }

  update(data) {
    if (this.holdKeys.length > 0) data.arpseqHoldKeys = this.holdKeys;
    for (const [k, v] of Object.entries(this.nameMap)) {
      if (Object.hasOwn(data, v)) this[k] = data[v];
    }
    this.phrase?.setTempo(this.tempo);
    this.setup();
  }

  holdSwitch() {
    this.holdState = !this.holdState;
    if (!this.holdState) this.setup();
    if (this.holdState) {
      this.pause();
      this.holdKeys = [];
    }
  }

  setup(
    pitchSequence = this.holdKeys,
    octaves = this.octaves,
    synthout = this.synthout,
    tempo = this.tempo,
    tick = this.callback,
  ) {
    if (pitchSequence.length === 0) return;
    this.dispose();

    const sequence = this.octaveExpandSequence(pitchSequence, octaves);

    let arpegioSequence = this.octaveExpandSequence(Array.from(new Set(pitchSequence)), octaves);
    arpegioSequence = arpegioSequence.sort((a, b) => a > b);
    arpegioSequence = arpegioSequence.concat(
      arpegioSequence.slice(1, arpegioSequence.length - 1).reverse(),
    );

    if (this.phrase) this.phrase.dispose();
    this.phrase = new Phrase({ sequence, arpegioSequence, synthout, tempo, tick });
    this.play();
  }

  octaveExpandSequence(sequence, octaves) {
    let seq = sequence;
    if (octaves !== 0) {
      for (let i = 0; i < Math.abs(octaves); i++) {
        const shift = octaves > 0 ? i + 1 : -(i + 1);
        seq = seq.concat(seq.map((p) => Scale.shiftOctave(p, shift)));
      }
    }
    return seq;
  }

  play(arpegio = this.arpegio) {
    if (this.phrase?.loop.state === 'started') {
      this.pause();
    } else {
      this.phrase?.start(0, arpegio);
    }
  }

  pause() {
    this.phrase?.stop();
  }

  dispose() {
    this.pause();
    this.phrase = this.phrase?.dispose();
  }
}

class Improviser {
  fibs = (() => {
    const fibs = (n = 200, xs = [0, 1]) => {
      if (n <= 0) return xs;
      const nextFib = xs[xs.length - 1] + xs[xs.length - 2];

      return fibs(n - 1, xs.concat(Math.min(nextFib, 1_000_000)));
    };
    return fibs();
  })();

  constructor(synthControl) {
    this.synthControl = synthControl;
    this.keyboard = Array.from(this.synthControl.container.children).map(
      (k) => k.querySelector('.keyName').innerText,
    );
    this.keyboardLength = this.keyboard.length;
    this.c = 0;

    this.speed = 2200;
    this.maxNoteDuration = 16000;

    this.minNoteDuration = 700;
    this.bassNoteRatio = 0.25;

    this.melodyChance = 0.8;

    this.maxMelodyDurationCap = 7000;
  }

  randGen = () => {
    if (this.keyboardLength === 0) return;

    const bassFibIndex = (this.c + Math.floor(Math.random() * 5)) % this.fibs.length;
    const bassRange = Math.max(1, Math.floor(this.keyboardLength * this.bassNoteRatio));

    const bassKeyIndex = this.fibs[bassFibIndex] % bassRange;
    const bassKey = this.keyboard[bassKeyIndex];

    if (bassKey) {
      const bassPitchRatio = bassKeyIndex / (this.keyboardLength - 1);
      let bassDuration =
        this.maxNoteDuration - bassPitchRatio * (this.maxNoteDuration - this.minNoteDuration);
      bassDuration *= 0.9 + Math.random() * 0.2;

      bassDuration = Math.max(10000, Math.min(bassDuration, 18000));

      this.play(0, bassDuration, bassKey);
    }

    if (Math.random() < this.melodyChance) {
      const melodyFibIndex = (this.c + 10 + Math.floor(Math.random() * 7)) % this.fibs.length;
      const melodyRange = this.keyboardLength - bassRange;
      if (melodyRange > 0) {
        const melodyIndexOffset = this.fibs[melodyFibIndex] % melodyRange;
        const melodyKeyIndex = bassRange + melodyIndexOffset;
        const melodyKey = this.keyboard[melodyKeyIndex];

        if (melodyKey) {
          const melodyPitchRatio = melodyKeyIndex / (this.keyboardLength - 1);
          let melodyDuration =
            this.maxNoteDuration - melodyPitchRatio * (this.maxNoteDuration - this.minNoteDuration);

          melodyDuration *= 0.85 + Math.random() * 0.35;

          melodyDuration = Math.max(
            this.minNoteDuration,
            Math.min(melodyDuration, this.maxMelodyDurationCap),
          );

          const melodyDelay = 100 + Math.random() * 100;
          this.play(melodyDelay, melodyDuration, melodyKey);
        }
      }
    }

    this.c = (this.c + 1) % this.fibs.length;

    const cycleTimeVariation = (Math.random() - 0.5) * (this.speed / 5);

    const nextCycleDelay = Math.max(this.speed * 0.7, this.speed + cycleTimeVariation);

    setTimeout(this.randGen, nextCycleDelay);
  };

  play(delay, duration, key) {
    const clampedDuration = Math.max(50, duration);
    setTimeout(() => {
      this.synthControl.handleKeyDown({ key });
      setTimeout(() => {
        this.synthControl.handleKeyUp({ key });
      }, clampedDuration);
    }, delay);
  }
}

// https://github.com/Tonejs/Presets/blob/gh-pages/instrument/Synth/Marimba.json
// https://github.com/therewasaguy/aqwertyon-1/blob/e592028a9d0dac7c11412b2ac68c096b6aac5cfd/lib/Tone.Preset.js

Tone.Synth.prototype.preset = {
  Moog01: {
    oscillator: {
      type: 'fatsawtooth',
      count: 4,
      spread: 30,
    },
    envelope: { attack: 0.01, decay: 0.1, sustain: 0.3, release: 0.5 },
  },
  Moog02: {
    oscillator: {
      type: 'fatsawtooth',
      count: 8,
      spread: 30,
    },
    envelope: { attack: 0.01, decay: 0.1, sustain: 0.3, release: 0.5 },
  },
  Moog03: {
    oscillator: {
      type: 'fatsawtooth',
      count: 16,
      spread: 50,
    },
    envelope: { attack: 0.01, decay: 0.1, sustain: 0.3, release: 0.5 },
  },
  Marimba: {
    oscillator: {
      partials: [1, 0, 2, 0, 3],
    },
    envelope: {
      attack: 0.001,
      decay: 1.2,
      sustain: 0,
      release: 1.2,
    },
  }
};
Tone.DuoSynth.prototype.preset = {
  Steely: {
    vibratoAmount: 0.0,
    vibratoRate: 5,
    vibratoDelay: 1,
    portamento: 0,
    harmonicity: 1.99,
    voice0: {
      volume: -8,
      portamento: 0,
      oscType: 'square',
      filter: {
        Q: 2,
        type: 'lowpass',
        rolloff: -12,
      },
      envelope: {
        attack: 0.01,
        decay: 1,
        sustain: 0,
        release: 0.4,
      },
      filterEnvelope: {
        attack: 0.001,
        decay: 0.01,
        sustain: 0.35,
        release: 1,
        min: 20,
        max: 8000,
      },
    },
    voice1: {
      volume: -1,
      portamento: 0,
      oscType: 'sine',
      filter: {
        Q: 2,
        type: 'lowpass',
        rolloff: -12,
      },
      envelope: {
        attack: 0.25,
        decay: 4,
        sustain: 0,
        release: 0.8,
      },
      filterEnvelope: {
        attack: 0.03,
        decay: 0.25,
        sustain: 0.7,
        release: 1,
        min: 1000,
        max: 2500,
      },
    },
  },
  Unicorn: {
    vibratoAmount: 0.5,
    vibratoRate: 5,
    vibratoDelay: 1,
    portamento: 0.1,
    harmonicity: 1.005,
    voice0: {
      volume: -2,
      portamento: 0,
      oscType: 'sawtooth',
      filter: {
        Q: 1,
        type: 'lowpass',
        rolloff: -24,
      },
      envelope: {
        attack: 0.01,
        decay: 0.25,
        sustain: 0.4,
        release: 1.2,
      },
      filterEnvelope: {
        attack: 0.001,
        decay: 0.05,
        sustain: 0.3,
        release: 2,
        min: 100,
        max: 10000,
      },
    },
    voice1: {
      volume: 0,
      portamento: 0,
      oscType: 'sawtooth',
      filter: {
        Q: 20,
        type: 'highpass',
        rolloff: -12,
      },
      envelope: {
        attack: 0.25,
        decay: 4,
        sustain: 0.1,
        release: 0.8,
      },
      filterEnvelope: {
        attack: 0.5,
        decay: 0.05,
        sustain: 0.7,
        release: 2,
        min: 5000,
        max: 2000,
      },
    },
  },
  Organ: {
    vibratoAmount: 0.5,
    vibratoRate: 5,
    vibratoDelay: 0,
    portamento: 0.001,
    harmonicity: 3.005,
    filter: {
      Q: 0.4,
      type: 'lowpass',
      rolloff: -12,
    },
    voice0: {
      volume: -7,
      portamento: 0.01,
      oscType: 'sine',
      envelope: {
        attack: 0.005,
        decay: 0.25,
        sustain: 0.4,
        release: 1.2,
      },
      filterEnvelope: {
        attack: 0.005,
        decay: 0.5,
        sustain: 0.7,
        release: 2,
        min: 800,
        max: 2200,
      },
    },
    voice1: {
      volume: -5,
      portamento: 0.01,
      oscType: 'sine',
      envelope: {
        attack: 0.25,
        decay: 4,
        sustain: 0.1,
        release: 0.8,
      },
      filterEnvelope: {
        attack: 0.005,
        decay: 0.5,
        sustain: 0.7,
        release: 2,
        min: 900,
        max: 4200,
      },
    },
  },
};

Tone.FMSynth.prototype.preset = {
  Saxophone: {
    portamento: 0.05,
    harmonicity: 1.5,
    modulationIndex: 14,
    oscillator: {
      type: 'sawtooth',
    },
    envelope: {
      attack: 0.06,
      decay: 0.05,
      sustain: 0.85,
      release: 0.35,
      attackCurve: 'exponential',
    },
    modulation: {
      type: 'triangle',
    },
    modulationEnvelope: {
      attack: 0.03,
      decay: 0.25,
      sustain: 0.4,
      release: 0.1,
      attackCurve: 'linear',
    },
    volume: -10,
  },
  Trumpet: {
    portamento: 0,
    harmonicity: 1,
    modulationIndex: 4,
    carrier: {
      volume: 0,
      portamento: 0,
      oscType: 'square',
      filter: {
        Q: 2,
        type: 'lowpass',
        rolloff: -12,
      },
      envelope: {
        attack: 0.01,
        decay: 1,
        sustain: 0.7,
        release: 0.4,
      },
      filterEnvelope: {
        attack: 0.06,
        decay: 0.07,
        sustain: 0.35,
        release: 0.8,
        min: 3000,
        max: 6500,
      },
    },
    modulator: {
      volume: -6,
      portamento: 0,
      oscType: 'triangle',
      filter: {
        Q: 0,
        type: 'lowpass',
        rolloff: -12,
      },
      envelope: {
        attack: 0.15,
        decay: 0.3,
        sustain: 1,
        release: 1.5,
      },
      filterEnvelope: {
        attack: 0.03,
        decay: 0.25,
        sustain: 0.7,
        release: 1,
        min: 20000,
        max: 20000,
      },
    },
  },
  Koto: {
    portamento: 0,
    harmonicity: 3.01,
    modulationIndex: 12.7,
    carrier: {
      volume: 0,
      portamento: 0,
      oscType: 'triangle',
      filter: {
        Q: 2,
        type: 'lowpass',
        rolloff: -12,
      },
      envelope: {
        attack: 0.01,
        decay: 2,
        sustain: 0,
        release: 0.8,
      },
      filterEnvelope: {
        attack: 0.06,
        decay: 0.07,
        sustain: 0.35,
        release: 0.8,
        min: 20000,
        max: 20000,
      },
    },
    modulator: {
      volume: -1,
      portamento: 0,
      oscType: 'sine',
      filter: {
        Q: 0,
        type: 'lowpass',
        rolloff: -12,
      },
      envelope: {
        attack: 0.02,
        decay: 2,
        sustain: 0,
        release: 0.8,
      },
      filterEnvelope: {
        attack: 0.03,
        decay: 0.25,
        sustain: 0.7,
        release: 1,
        min: 20000,
        max: 20000,
      },
    },
  },
  ScratchAttack: {
    portamento: 0,
    harmonicity: 10,
    modulationIndex: 50,
    carrier: {
      volume: 0,
      portamento: 0,
      oscType: 'square',
      filter: {
        Q: 2,
        type: 'lowpass',
        rolloff: -12,
      },
      envelope: {
        attack: 0.08,
        decay: 0.3,
        sustain: 0,
        release: 1.2,
      },
      filterEnvelope: {
        attack: 0.01,
        decay: 0.1,
        sustain: 0,
        release: 0.2,
        min: 200,
        max: 10000,
      },
    },
    modulator: {
      volume: -6,
      portamento: 0,
      oscType: 'sine',
      filter: {
        Q: 1,
        type: 'highpass',
        rolloff: -48,
      },
      envelope: {
        attack: 0.1,
        decay: 0.2,
        sustain: 0.3,
        release: 0.01,
      },
      filterEnvelope: {
        attack: 0.2,
        decay: 0.2,
        sustain: 0.8,
        release: 0.01,
        min: 20,
        max: 2000,
      },
    },
  },
  DistGit: {
    portamento: 0,
    harmonicity: 1,
    modulationIndex: 10,
    carrier: {
      volume: 0,
      portamento: 0,
      oscType: 'square',
      filter: {
        Q: 2,
        type: 'lowpass',
        rolloff: -12,
      },
      envelope: {
        attack: 0.001,
        decay: 3.3,
        sustain: 0,
        release: 1.2,
      },
      filterEnvelope: {
        attack: 0.05,
        decay: 0.15,
        sustain: 1,
        release: 1.5,
        min: 400,
        max: 4000,
      },
    },
    modulator: {
      volume: -3,
      portamento: 0,
      oscType: 'sine',
      filter: {
        Q: 1,
        type: 'lowpass',
        rolloff: -48,
      },
      envelope: {
        attack: 0.3,
        decay: 0.4,
        sustain: 1,
        release: 1.7,
      },
      filterEnvelope: {
        attack: 0.02,
        decay: 0.02,
        sustain: 0.1,
        release: 1.5,
        min: 200,
        max: 200,
      },
    },
  },
};

Tone.MonoSynth.prototype.preset = {
  Ocarina: {
    portamento: 0.05,
    oscillator: {
      type: 'fatsine',
      spread: 15,
      count: 3,
    },
    envelope: {
      attack: 0.06,
      decay: 0.02,
      sustain: 0.9,
      release: 0.3,
      attackCurve: 'linear',
      releaseCurve: 'exponential',
    },
    filter: {
      type: 'lowpass',
      frequency: 10000,
      Q: 1,
    },
    filterEnvelope: {
      attack: 0.01,
      decay: 0.1,
      sustain: 0.8,
      release: 0.5,
      baseFrequency: 2000,
      octaves: 2.5,
    },
    volume: -8,
  },
  Drone: {
    oscillator: {
      type: 'fmsquare5',
      modulationType: 'sine',
      modulationIndex: 2,
      harmonicity: 0.501,
    },
    filter: {
      Q: 1,
      type: 'lowpass',
      rolloff: -12,
    },
    envelope: {
      attack: 1.0,
      decay: 0.1,
      sustain: .5,
      release: 15,
    },
    filterEnvelope: {
      attack: 0.01,
      decay: 0.1,
      sustain: 1,
      release: 15,
      baseFrequency: 50,
      octaves: 4.4,
    },
  },
  BassGuitar: {
    oscillator: {
      type: 'fmsquare5',
      modulationType: 'triangle',
      modulationIndex: 2,
      harmonicity: 0.501,
    },
    filter: {
      Q: 1,
      type: 'lowpass',
      rolloff: -24,
    },
    envelope: {
      attack: 0.01,
      decay: 0.1,
      sustain: 0.4,
      release: 2,
    },
    filterEnvelope: {
      attack: 0.01,
      decay: 0.1,
      sustain: 0.8,
      release: 1.5,
      baseFrequency: 50,
      octaves: 4.4,
    },
  },
  Pianoetta: {
    portamento: 0.0,
    oscType: 'triangle',
    filter: {
      Q: 2,
      type: 'lowpass',
      rolloff: -48,
    },
    envelope: {
      attack: 0.01,
      decay: 3,
      sustain: 0,
      release: 0.45,
    },
    filterEnvelope: {
      attack: 0.001,
      decay: 0.32,
      sustain: 0.9,
      release: 3,
      min: 700,
      max: 3500,
    },
  },
  Barky: {
    portamento: 0.01,
    oscType: 'triangle',
    filter: {
      Q: 3,
      type: 'highpass',
      rolloff: -12,
    },
    envelope: {
      attack: 0.05,
      decay: 0.15,
      sustain: 0.6,
      release: 1,
    },
    filterEnvelope: {
      attack: 0.02,
      decay: 0.2,
      sustain: 0.8,
      release: 1.5,
      min: 3000,
      max: 250,
    },
  },
  Bassy: {
    portamento: 0.0,
    oscType: 'square',
    filter: {
      Q: 1,
      type: 'lowpass',
      rolloff: -24,
    },
    envelope: {
      attack: 0.01,
      decay: 0.06,
      sustain: 0.4,
      release: 1,
    },
    filterEnvelope: {
      attack: 0.01,
      decay: 0.04,
      sustain: 0.6,
      release: 1.5,
      min: 60,
      max: 720,
    },
  },
  BrassCircuit: {
    portamento: 0.01,
    oscType: 'sawtooth',
    filter: {
      Q: 0.8,
      type: 'lowpass',
      rolloff: -24,
    },
    envelope: {
      attack: 0.9,
      decay: 0.4,
      sustain: 0.6,
      release: 0.5,
    },
    filterEnvelope: {
      attack: 0.05,
      decay: 0.8,
      sustain: 0.4,
      release: 1.5,
      min: 2000,
      max: 5000,
    },
  },
  Pizz: {
    portamento: 0.0,
    oscType: 'square',
    filter: {
      Q: 2,
      type: 'highpass',
      rolloff: -24,
    },
    envelope: {
      attack: 0.01,
      decay: 0.2,
      sustain: 0.0,
      release: 0.2,
    },
    filterEnvelope: {
      attack: 0.01,
      decay: 0.1,
      sustain: 0.0,
      release: 0.1,
      min: 900,
      max: 100,
    },
  },
  Kick: {
    portamento: 0.0,
    oscType: 'square',
    filter: {
      Q: 2,
      type: 'bandpass',
      rolloff: -12,
    },
    envelope: {
      attack: 0.01,
      decay: 0.2,
      sustain: 0.0,
      release: 0.2,
    },
    filterEnvelope: {
      attack: 0.01,
      decay: 0.2,
      sustain: 1,
      release: 0.4,
      min: 3000,
      max: 30,
    },
  },
  LaserSteps: {
    portamento: 0.0,
    oscType: 'sawtooth',
    filter: {
      Q: 2,
      type: 'bandpass',
      rolloff: -24,
    },
    envelope: {
      attack: 0.01,
      decay: 0.1,
      sustain: 0.2,
      release: 0.6,
    },
    filterEnvelope: {
      attack: 0.02,
      decay: 0.4,
      sustain: 1,
      release: 0.2,
      min: 0,
      max: 7500,
    },
  },
};

class Synthesizer {
  effects = {
    distortion: new Tone.Distortion(0),
    chorus: new Tone.Chorus({ frequency: 1.5, delayTime: 3.5, depth: 0 }).start(),
    vibrato: new Tone.Vibrato({ frequency: 5, depth: 0.1 }),
    tremolo: new Tone.Tremolo({ frequency: 9, depth: 0.75 }).start(),
    delay: new Tone.FeedbackDelay({ delayTime: 0, feedback: 0.5 }),
    reverb: new Tone.Reverb(0.1),
    lowpass: new Tone.Filter({ frequency: 0, type: 'lowpass', Q: 10 }),
    compressor: new Tone.Compressor({
      threshold: -30,
      ratio: 20,
      attack: 0.003,
      release: 0.25,
    }),
  };

  velocity = 0.5;
  synthOutput = new Tone.Gain(0.5);
  effectsOutput = new Tone.Gain(0.5);
  masterMix = new Tone.Gain(1).toDestination();

  setInstrument(name) {
    const [synthName, presetName] = name.split('.');
    const synth = new Tone.PolySynth(Tone[synthName]);
    if (presetName) {
      synth.set(Tone[synthName].prototype.preset[presetName]);
    }
    return synth;
  }

  constructor({ synthType = 'Moog01' } = {}) {
    Tone.start();
    Tone.getTransport().start();

    this.synth = this.setInstrument(synthType);
    this.synth.connect(this.synthOutput);
    this.setupEffectChain(this.effectsOutput);
    this.effectsOutput.connect(this.masterMix);
    this.synthOutput.connect(this.masterMix);
    this.masterMix.toDestination();
  }

  setupEffectChain(destination) {
    this.synth.chain(...Object.values(this.effects), destination);
  }

  getContextAndDestination() {
    const audioContext = Tone.getContext();
    const destination = audioContext.createMediaStreamDestination();
    this.setupEffectChain(destination);
    return { audioContext, destination };
  }

  triggerAttack(note, velocity = this.velocity) {
    this.synth.triggerAttack(note, undefined, velocity);
  }

  triggerAttackRelease(note, duration, time, velocity = this.velocity) {
    this.synth.triggerAttackRelease(note, duration, time, velocity);
  }

  triggerRelease(note) {
    this.synth.triggerRelease(note);
  }

  updateEffects({
    decay,
    delayTime,
    distortion,
    tremoloFreq,
    tremoloDepth,
    vibratoFreq,
    vibratoDepth,
    chorusFreq,
    chorusDepth,
    chorusDelayTime,
    lowpass,
    velocity,
    volume,
    gain,
    volumeMix,
    compressorRatio,
  } = {}) {
    this.effects.reverb.set({ decay });
    this.effects.reverb.wet.value = decay > 0.1 ? 1 : 0;

    this.effects.delay.set({ delayTime });
    this.effects.delay.wet.value = delayTime > 0 ? 1 : 0;

    this.effects.lowpass.set({ frequency: lowpass });

    this.effects.distortion.set({ distortion });
    this.effects.distortion.wet.value = distortion > 0 ? 1 : 0;

    this.effects.tremolo.set({
      frequency: tremoloFreq,
      depth: tremoloDepth,
    });
    this.effects.tremolo.wet.value = tremoloDepth > 0 ? 1 : 0;

    this.effects.vibrato.set({
      frequency: vibratoFreq,
      depth: vibratoDepth,
    });
    this.effects.vibrato.wet.value = vibratoDepth > 0.1 ? 1 : 0;

    this.effects.chorus.set({
      frequency: chorusFreq,
      delayTime: chorusDelayTime,
      depth: chorusDepth,
    });
    this.effects.chorus.wet.value = chorusDepth > 0 ? 1 : 0;

    this.effects.compressor.set({ ratio: compressorRatio });

    this.velocity = velocity;
    this.synth.volume.value = volume;
    this.synthOutput.gain.value = volumeMix;
    this.effectsOutput.gain.value = 1 - volumeMix;
    this.masterMix.gain.value = gain;
  }

  dispose() {
    [
      ...Object.values(this.effects),
      this.synth,
      this.synthOutput,
      this.effectsOutput,
      this.masterMix,
    ].forEach((e) => e?.dispose());
  }
}

class SynthControl {
  container = document.querySelector('.keys-container');
  activeMouseButton = false;
  currentActiveKey = null;

  constructor(scale, synthType, webglEngine) {
    this.webglEngine = webglEngine;
    this.scale = scale;
    this.setKeyboard();
    this.synth = new Synthesizer({ synthType });
    this.arpseq = new ArpSeq(this.synth, () => {
      this.webglEngine.randomPosAndLook();
      this.webglEngine.vertigoEnable(-1);
    });
    this.render();
    this.improviser = new Improviser(this);
  }

  setKeyboard() {
    this.keys = '1234567890qwertyuiopasdfghjklzxcvbnm'.split('');
    this.keyboard = this.keys.map((key, i) => {
      const { note, pitch } = this.scale.scale.range[i % this.scale.scale.range.length];
      return { key, active: false, note, pitch };
    });
  }

  handleKeyPress(event) {
    if (event.code === 'Space') {
      this.arpseq.holdSwitch();
    }
  }

  handleKeyDown(event) {
    const i = this.keys.indexOf(event.key.toLowerCase());
    if (i === -1) return;
    this.pressKeyDown(this.keyboard[i]);
  }

  handleKeyUp(event) {
    const i = this.keys.indexOf(event.key.toLowerCase());
    if (i === -1) return;
    this.pressKeyUp(this.keyboard[i]);
  }

  pressKeyDown(key) {
    if (key.active) return;
    if (this.arpseq.holdState) this.arpseq.holdKeys.push(key.pitch);
    this.webglEngine.randomPosAndLook();
    key.active = true;
    this.synth.triggerAttack(key.pitch);
    this.switchActiveNote(key, true);
  }

  pressKeyUp(key) {
    if (key.active) this.webglEngine.vertigoEnable(-1);
    key.active = false;
    this.synth.triggerRelease(key.pitch);
    this.switchActiveNote(key, false);
  }

  render() {
    this.keyboard.forEach((key) => {
      const keyHTML = `
        <button class='key'>
          <div class='noteName'>${key.note}</div>
          <div class='keyName'>${key.key}</div>
        </button>`;
      this.container.insertAdjacentHTML('beforeend', keyHTML);
      key.el = this.container.lastElementChild;
    });
  }

  switchActiveNote(key, state) {
    key.el.classList.toggle('pressed', state);
  }

  dispose() {
    this.container.innerHTML = '';
    this.synth?.dispose();
    this.arpseq?.dispose();
  }

  handleContainerMouseDown(event) {
    if (!event.target.classList.contains('key')) return;
    this.activeMouseButton = true;
    const key = this.findKeyByElement(event.target);
    if (key) {
      this.currentActiveKey = key;
      this.pressKeyDown(key);
    }
  }

  handleContainerMouseUp() {
    this.activeMouseButton = false;
    if (this.currentActiveKey) {
      this.pressKeyUp(this.currentActiveKey);
      this.currentActiveKey = null;
    }
  }

  handleContainerMouseOut(event) {
    if (this.activeMouseButton && !this.container.contains(event.relatedTarget)) {
      this.pressKeyUp(this.currentActiveKey);
      this.currentActiveKey = null;
      this.activeMouseButton = false;
    }
  }

  findKeyByElement(element) {
    return this.keyboard.find((key) => key.el === element);
  }
}

class TapeLooper {
  tapeEl = document.getElementById('tape');
  mediaRecorder = null;
  audioChunks = [];
  tapeState = 0;
  options = {
    audioBitsPerSecond: 3200000,
    mimeType: 'audio/webm',
  };
  lastBlob = window?.tapeStorage || '';

  dispose() {
    this.mediaRecorder = null;
    this.audioChunks = [];
    this.tapeState = 0;
    this.tapeEl.src = '';
    this.lastBlob = '';
  }

  togglePlay() {
    if (this.tapeEl.paused) {
      this.tapeEl.play();
    } else {
      this.tapeEl.pause();
    }
  }

  constructor(synthControl) {
    this.synthControl = synthControl;
    if (this.lastBlob) this.tapeEl.src = this.lastBlob;
  }

  handleEvents(event) {
    if (event.key === 'Backspace') {
      this.dispose();
      window.tapeStorage = undefined;
    }
    if (event.key === '-') {
      if (this.tapeState === 0) {
        this.startRecording();
      } else {
        this.stopRecording();
      }
    }
    if (event.key === '=') {
      this.togglePlay();
    }
  }

  createProxyAudio(audioElement, followBehaviour = false) {
    const proxyAudioElement = new Audio(audioElement.src);
    proxyAudioElement.loop = true;
    proxyAudioElement.currentTime = audioElement.currentTime;
    proxyAudioElement.volume = audioElement.volume;
    proxyAudioElement.playbackRate = audioElement.playbackRate;

    if (followBehaviour) {
      audioElement.onplay = () => {
        // hell do not touch fuck it
        // if (audioElement.src !== proxyAudioElement.src) {
        //   proxyAudioElement = this.createProxyAudio(audioElement, followBehaviour);
        // }
        proxyAudioElement.play();
      };

      audioElement.onpause = () => {
        proxyAudioElement.pause();
      };
    }

    return proxyAudioElement;
  }

  async startRecording() {
    this.tapeState = 1;
    this.tapeEl.style.border = '2px solid red';

    const { audioContext, destination } = this.synthControl.synth.getContextAndDestination();

    if (this.lastBlob) {
      const audioElement = new Audio(this.lastBlob);
      const sourceNode = audioContext.createMediaElementSource(audioElement);
      sourceNode.connect(destination);
      audioElement.loop = true;
      // audioElement.currentTime = this.tapeEl.currentTime;
      audioElement.play();
      this.tapeEl.play();
    }

    this.mediaRecorder = new MediaRecorder(destination.stream, this.options);
    this.audioChunks = [];

    this.mediaRecorder.ondataavailable = (event) => {
      if (event.data.size > 0) {
        this.audioChunks.push(event.data);
      }
    };

    this.mediaRecorder.onstop = () => {
      const audioBlob = new Blob(this.audioChunks, { type: 'audio/webm' });
      this.lastBlob = URL.createObjectURL(audioBlob);
      this.tapeEl.src = this.lastBlob;
      window.tapeStorage = this.lastBlob;
      this.tapeEl.play();
    };
    this.mediaRecorder.start();
  }

  stopRecording() {
    this.tapeState = 0;
    this.tapeEl.style.border = 'none';
    if (this.mediaRecorder?.state === 'recording') {
      this.mediaRecorder.stop();
    }
  }
}

class ArrowKeyRangeControl {
  detuneInterval = null;
  detuneReleaseInterval = null;

  constructor(
    inputElement,
    onChangeCallback,
    step = 2,
    intervalDelay = 4,
    releaseIntervalDelay = 4,
  ) {
    this.inputElement = inputElement;
    this.value = 0;
    this.max = Number.parseFloat(this.inputElement.max);
    this.min = Number.parseFloat(this.inputElement.min);

    this.onChangeCallback = onChangeCallback;
    this.step = step;
    this.intervalDelay = intervalDelay;
    this.releaseIntervalDelay = releaseIntervalDelay;
  }

  handleKeyDown = (event) => {
    if (event.key === 'ArrowUp' || event.key === 'ArrowDown') {
      event.preventDefault();
      if (!this.detuneInterval) {
        this.dispose();
        this.value = Number.parseFloat(this.inputElement.value);
        this.direction = event.key === 'ArrowDown' ? -1 : 1;
        this.detuneInterval = setInterval(this.attack, this.intervalDelay);
      }
    }
  };

  attack = () => {
    const oldValue = this.value;
    this.value = clamp(oldValue + this.step * this.direction, this.min, this.max);
    this.render(oldValue);
  };

  release = () => {
    const oldValue = this.value;
    this.value = Math.max(0, Math.abs(oldValue) - this.step) * this.direction;
    if (this.value === 0) this.dispose();
    this.render(oldValue);
  };

  render(oldValue) {
    if (this.value === oldValue) return;
    this.inputElement.value = this.value;
    this.onChangeCallback(this.value);
  }

  handleKeyUp = (event) => {
    if (
      (event.key === 'ArrowUp' ||
        event.key === 'ArrowDown' ||
        event.target === this.inputElement) &&
      !this.detuneReleaseInterval
    ) {
      this.dispose();
      this.value = Number.parseFloat(this.inputElement.value);
      this.direction = this.value > 0 ? 1 : -1;
      if (this.value !== 0) {
        this.detuneReleaseInterval = setInterval(this.release, this.releaseIntervalDelay);
      }
    }
  };

  dispose() {
    clearInterval(this.detuneInterval);
    this.detuneInterval = null;
    clearInterval(this.detuneReleaseInterval);
    this.detuneReleaseInterval = null;
  }
}

function download(blob, name) {
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = name;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}

function easeInExpo(x, power = 3.14) {
  return x === 0 ? 0 : power ** (10 * x - 10);
}

function parseBigInputRange(input) {
  const value = Number.parseFloat(input.value);
  const min = Number.parseFloat(input.min);
  const max = Number.parseFloat(input.max);
  return clamp(easeInExpo(value / max) * max, min, max);
}

function strOrObj(v) {
  try {
    return JSON.parse(v);
  } catch (e) {
    return String(v);
  }
}

class Recorder {
  canvasElement = document.querySelector('canvas');
  mediaRecorder = null;
  videoChunks = [];
  startTime = 0;

  options = {
    audioBitsPerSecond: 320000,
    videoBitsPerSecond: 6500000,
    mimeType: 'video/webm',
  };

  constructor(synthControl, tapeLooper) {
    this.tapeLooper = tapeLooper;
    this.synthControl = synthControl;
  }

  start = (audioOnly) => {
    const tracks = [];
    if (!audioOnly) {
      const canvasStream = this.canvasElement.captureStream(24);
      tracks.push(...canvasStream.getTracks());
    }

    const { audioContext, destination } = this.synthControl.synth.getContextAndDestination();

    if (this.tapeLooper.lastBlob) {
      const proxyTape = this.tapeLooper.createProxyAudio(this.tapeLooper.tapeEl, true);
      const sourceNode = audioContext.createMediaElementSource(proxyTape);
      sourceNode.connect(destination);
      if (!this.tapeLooper.tapeEl.paused) proxyTape.play();
    }

    tracks.push(...destination.stream.getTracks());

    const combinedStream = new MediaStream(tracks);

    this.mediaRecorder = new MediaRecorder(combinedStream, this.options);

    this.mediaRecorder.ondataavailable = (event) => {
      if (event.data.size > 0) {
        this.videoChunks.push(event.data);
      }
    };

    this.mediaRecorder.onstop = async () => {
      const recBlob = new Blob(this.videoChunks, { type: 'video/webm' });
      const duration = Date.now() - this.startTime;
      const blob = await window.ysFixWebmDuration(recBlob, duration);
      download(blob, `rec_${Date.now()}.webm`);
      this.dispose();
    };

    this.mediaRecorder.start();
    this.startTime = Date.now();
  };

  stop() {
    if (this.mediaRecorder && this.mediaRecorder.state === 'recording') {
      this.mediaRecorder.stop();
    }
  }

  dispose() {
    this.mediaRecorder = null;
    this.videoChunks = [];
    this.startTime = 0;
  }
}

const webglEngine = new WebglEngine();
let synthControl = undefined;
let recorder = undefined;
let tapeLooper = undefined;
const data = {};

function parseParams() {
  const url = new URL(window.location.href);
  url.searchParams.forEach((v, k) => {
    Object.assign(data, { [k]: strOrObj(v) });
    const inputEl = document.querySelector(`#${k}`);
    if (inputEl) inputEl.value = v;
  });
}

function handleInput(input) {
  if (input.type === 'range') {
    return input.id.includes('Freq') ? parseBigInputRange(input) : Number.parseFloat(input.value);
  }
  if (input.type === 'number') return Number.parseFloat(input.value);
  if (input.type === 'checkbox') return Boolean(input.checked);
  if (input.type === 'radio') {
    data.arpseqArpegiator = document.querySelector('#arpseqArpegiator').checked;
    return input.checked;
  }
  if (input.tagName === 'SELECT') return input.value;
  return undefined;
}

function setInput(input) {
  if (input.id.includes('detune')) return;
  const result = handleInput(input);
  if (result === undefined) return;
  Object.assign(data, { [input.id]: result });
  synthControl?.synth.updateEffects(data);
  synthControl?.arpseq.update(data);
}

function checkInputs() {
  const dataIsEmpty = Object.keys(data).length === 0;
  document.querySelectorAll('input, select').forEach(setInput);
  if (dataIsEmpty) parseParams();
}

document.body.addEventListener('input', (event) => {
  setInput(event.target);
  if (/number|checkbox/.test(event.target.type) || event.target.tagName === 'SELECT') {
    setSynthControl();
  }
});

const detuneInput = document.getElementById('detune-amount');
const arrowKeyController = new ArrowKeyRangeControl(detuneInput, (detune) => {
  synthControl?.synth.synth.set({ detune });
  requestAnimationFrame(() => {
    const value = detune / 100;
    document.body.firstElementChild.style.filter = `invert(${value})`;
    webglEngine.setUniforms(value);
  });
});
window.addEventListener('keydown', arrowKeyController.handleKeyDown);
window.addEventListener('keyup', arrowKeyController.handleKeyUp);
detuneInput.addEventListener('change', arrowKeyController.handleKeyUp);

function setSynthControl() {
  checkInputs();
  const { key, mode, octave, prepared, pitch } = data;
  const scale = new Scale(key, mode, octave, prepared, pitch);

  synthControl?.dispose();
  tapeLooper?.dispose();
  recorder?.dispose();

  synthControl = new SynthControl(scale, data.synth, webglEngine);
  synthControl?.synth.updateEffects(data);

  if (Array.isArray(data.arpseqHoldKeys)) {
    synthControl?.arpseq.update(data, true);
    synthControl?.arpseq.pause();
  }

  tapeLooper = new TapeLooper(synthControl);
  recorder = new Recorder(synthControl, tapeLooper);
}

const recordButton = document.getElementById('record-button');
const stopButton = document.getElementById('stop-button');
const audioOnly = document.querySelector('#audio-only');
function toggleButtons() {
  recordButton.disabled = !recordButton.disabled;
  stopButton.disabled = !stopButton.disabled;
}
recordButton.addEventListener('click', () => {
  toggleButtons();
  recorder.start(audioOnly.checked);
});
stopButton.addEventListener('click', () => {
  recorder.stop();
  toggleButtons();
});

window.addEventListener('keydown', (e) => tapeLooper?.handleEvents(e));

window.addEventListener('keypress', (e) => synthControl?.handleKeyPress(e));
window.addEventListener('keydown', (e) => synthControl?.handleKeyDown(e));
window.addEventListener('keyup', (e) => synthControl?.handleKeyUp(e));

const keysContainer = document.querySelector('.keys-container');
keysContainer.addEventListener('mousedown', (e) => synthControl?.handleContainerMouseDown(e));
keysContainer.addEventListener('mouseup', (e) => synthControl?.handleContainerMouseUp(e));
keysContainer.addEventListener('mouseleave', (e) => synthControl?.handleContainerMouseOut(e));

document.addEventListener('DOMContentLoaded', setSynthControl);

document
  .querySelector('#randomGenerator')
  .addEventListener('click', () => synthControl?.improviser?.randGen(), { once: true });

document
  .querySelector('#playBtnArpseq')
  .addEventListener('click', () => synthControl?.arpseq.play());
document
  .querySelector('#holdBtnArpseq')
  .addEventListener('click', () => synthControl?.arpseq.holdSwitch());

document.querySelector('#share').addEventListener('click', () => {
  Object.assign(data, synthControl.arpseq.export());
  const url = new URL(window.location.href);
  url.search = '';
  Object.entries(data).forEach(([k, v]) => {
    const encodedValue = typeof v === 'string' ? v : JSON.stringify(v);
    url.searchParams.append(k, encodedValue);
  });
  navigator.clipboard.writeText(url.href);
});
