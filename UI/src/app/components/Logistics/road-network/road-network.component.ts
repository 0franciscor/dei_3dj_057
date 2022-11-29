import { AfterViewInit, Component, ElementRef, Input, OnInit, ViewChild } from '@angular/core';
import * as THREE from 'three';
import { nodeData, warehousePosition } from './RoadNetworkJS/default-data';
import NodeTemplate from './RoadNetworkJS/node-template';
import roadNetworkTemplate from './RoadNetworkJS/road-network';
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";
import { RoadNetworkService } from 'src/app/Services/RoadNetworkService/road-network.service';
import {GLTFLoader} from 'three/examples/jsm/loaders/GLTFLoader'
import { DRACOLoader } from 'three/examples/jsm/loaders/DRACOLoader';
import { animate } from '@angular/animations';
import { Scene } from 'three';

 @Component({
  selector: 'app-road-network',
  templateUrl: './RoadNetworkJS/road-network.component.html',
  styleUrls: ['./road-network.component.css']
})


  

export class RoadNetworkComponent implements OnInit, AfterViewInit {

  
  @ViewChild('canvas') 
  private canvasRef!: ElementRef;

  // Cube properties
  @Input() public rotationSpeedX: number = 0.001;

  @Input() public rotationSpeedY: number = 0.005;
  
  @Input() public size: number = 200;
  
  @Input() public texture: string = "";



  //Stage properties
  @Input() public cameraZ: number = 8000;

  @Input() public fieldOfView: number = 1;
  
  @Input('nearClipping') public nearClippingPlane: number = 1;
  
  @Input('farClipping') public farClippingPlane: number = 100000;


  //Helper properties (private)

  private camera!: THREE.PerspectiveCamera;

  private get canvas(): HTMLCanvasElement {
    return this.canvasRef.nativeElement;
  }



 

  private renderer!: THREE.WebGLRenderer;

  private scene!: THREE.Scene;

  private roadNetwork !: roadNetworkTemplate;
  
  

  private async createScene() {

    let rnService = new RoadNetworkService();
    let warehouses: any[] = [];
    await rnService.getAllWarehouses().then((data) => {
      warehouses = data;
    });
    let paths: any[] = [];
    
    for( const warehouse of warehouses){

      await rnService.getPathBetweenWarehouses(warehouse.id).then((data) => {
        
        if(data != null){
          for (const element of data) {
            paths.push({startWHId:element.startWHId, destinationWHId:element.destinationWHId, roadWidth:this.getRandomNumber()});
          }
  
        }
      });

    }
    


    let positions = roadNetworkTemplate.calculatePositions(warehouses);
    this.roadNetwork = new roadNetworkTemplate({positions:positions,
      paths:paths});

      
    

   
    //Scene
    this.scene = new THREE.Scene();
    //const scene1 = new THREE.Scene();
    //i want to define a backgroud image for the scene
    const loader = new THREE.TextureLoader();
    loader.load('assets/sky.jpg', (texture) => {
      this.scene.background = texture;
    });
    this.scene.add(this.roadNetwork.object);
    


    /* //load road model
    const loader = new GLTFLoader();
    //this.scene.add(loader)
    const dracoLoader = new DRACOLoader();
    dracoLoader.setDecoderPath( '/examples/js/libs/draco/' );
    loader.setDRACOLoader( dracoLoader );
    
    loader.load('./assets/road/scene.gltf', 
      function ( gltf ) {
        console.log(gltf)
        scene1.add(gltf.scene)
        gltf.animations; // Array<THREE.AnimationClip>
        gltf.scene; // THREE.Group
        gltf.scenes; // Array<THREE.Group>
        gltf.cameras; // Array<THREE.Camera>
        gltf.asset; // Object
      },
      // called while loading is progressing
      function ( xhr ) {
      
        console.log( ( xhr.loaded / xhr.total * 100 ) + '% loaded' );
      
      },
      // called when loading has errors
      function ( error ) {
      
        console.log( 'An error happened' );
      
      }
    );
      this.scene.add(scene1)
    //lightning
    const light = new THREE.DirectionalLight(0xffffffff,1)
    light.position.set(2,2,5)
    this.scene.add(light); */


    //Camera
    this.camera = new THREE.PerspectiveCamera(
      this.fieldOfView,
      this.canvas.clientWidth / this.canvas.clientHeight,
      this.nearClippingPlane,
      this.farClippingPlane
    );

    this.camera.position.z = this.cameraZ;

  
  }

  private animateCircle() {
    // this.roadNetwork.object.rotation.x += this.rotationSpeedX;
    // this.roadNetwork.object.rotation.y += this.rotationSpeedY; 
  }
/* 
  //Animate road
  private animateRoad(){
    requestAnimationFrame(animate)
    this.renderer.render(this.scene,this.camera);
  }
 */
  
  
  private lastwindowWidth: number = window.innerWidth;
  private lastwindowHeight: number = window.innerHeight;
  private onWindowResize() {
    if(this.lastwindowWidth != window.innerWidth || this.lastwindowHeight != window.innerHeight){
      this.camera.aspect = window.innerWidth / window.innerHeight;
      this.camera.updateProjectionMatrix();
      this.renderer.setSize(window.innerWidth, window.innerHeight);
      this.lastwindowWidth = window.innerWidth;
      this.lastwindowHeight = window.innerHeight;
    }
    
  }

  private startRenderingLoop() {
    //Renderer
    
    this.renderer = new THREE.WebGLRenderer({ canvas: this.canvas });
    this.renderer.setPixelRatio(window.devicePixelRatio);
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight-64);

    let component: RoadNetworkComponent = this;
    (function render() {
      requestAnimationFrame(render);
      component.animateCircle();
      component.onWindowResize();
      component.renderer.render(component.scene, component.camera);
    }());

    new OrbitControls(this.camera,this.canvas);
  }


  async ngAfterViewInit() {
    await this.createScene();
    this.startRenderingLoop();
  }


  ngOnInit(): void {

  }
  

  private getRandomNumber() {
    //return random number between 0.1 and 0.8
    return Math.random() * (0.8 - 0.1) + 0.1;
}

}
