import { AfterViewInit, Component, ElementRef, Input, OnInit, ViewChild } from '@angular/core';
import * as THREE from 'three';
import { nodeData, warehousePosition } from './RoadNetworkJS/default-data';
import NodeTemplate from './RoadNetworkJS/node-template';
import roadNetworkTemplate from './RoadNetworkJS/road-network';
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";

 @Component({
  selector: 'app-road-network',
  templateUrl: './RoadNetworkJS/road-network.component.html',
  styleUrls: ['./road-network.component.css']
})
export class RoadNetworkComponent implements OnInit, AfterViewInit {

  @ViewChild('canvas') 
  private canvasRef!: ElementRef;

  // Cube properties
  @Input() public rotationSpeedX: number = 0.01;

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
  

  private createScene() {

    this.roadNetwork = new roadNetworkTemplate({positions:warehousePosition});
    
   
    //Scene
    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0x000000);
    this.scene.add(this.roadNetwork.object);
    
    
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



  private startRenderingLoop() {
    //Renderer
    
    this.renderer = new THREE.WebGLRenderer({ canvas: this.canvas });
    this.renderer.setPixelRatio(window.devicePixelRatio);
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight-64);

    let component: RoadNetworkComponent = this;
    (function render() {
      requestAnimationFrame(render);
      component.animateCircle();
      component.renderer.render(component.scene, component.camera);
    }());

    
    const controls = new OrbitControls(this.camera,this.canvas);

    

    

  }


  ngAfterViewInit() {
    this.createScene();
    this.startRenderingLoop();
  }


  ngOnInit(): void {

  }
  

}
