import { AfterViewInit, Component, ElementRef, Input, OnInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { RoadNetworkService } from 'src/app/Services/RoadNetworkService/road-network.service';
import * as THREE from 'three';
import { Object3D, Raycaster } from 'three';
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";
import Player from './RoadNetworkJS/player';
import roadNetworkTemplate from './RoadNetworkJS/road-network';
import truckNetowrk from './RoadNetworkJS/truck-network';





@Component({
  selector: 'app-road-network',
  templateUrl: './RoadNetworkJS/road-network.component.html',
  styleUrls: ['./road-network.component.css']
})
export class RoadNetworkComponent implements OnInit, AfterViewInit {

  constructor(private loginService:LoginService,private router: Router) { }


  @ViewChild('container')
  private containerRef!: ElementRef;

  // Cube properties
  @Input() public rotationSpeedX: number = 0.001;

  @Input() public rotationSpeedY: number = 0.005;

  @Input() public size: number = 200;

  @Input() public texture: string = "";



  //Stage properties
  @Input() public cameraZ: number = 8000;

  @Input() public fieldOfView: number = 1;

  @Input('nearClipping') public nearClippingPlane: number = 0.1;

  @Input('farClipping') public farClippingPlane: number = 100000;


  //Helper properties (private)

  private camera!: THREE.PerspectiveCamera;

  private get container(): HTMLDivElement {
    return this.containerRef.nativeElement;
  }
  
  private get canvas(): HTMLCanvasElement {
    return this.container.querySelector('canvas') as HTMLCanvasElement;
  }

  private get table(): HTMLTableElement {
    return this.container.querySelector('table') as HTMLTableElement;
  }

  //get select in table
  private get select(): HTMLSelectElement {
    return this.table.querySelector('select') as HTMLSelectElement;
  }

  private player!: Player;
  private closestWarehouse!: Object3D;
  private roads: Object3D[] = [];
  private warehouses: Object3D[] = [];

  private renderer!: THREE.WebGLRenderer;

  private scene!: THREE.Scene;

  private roadNetwork !: roadNetworkTemplate;
  private truckNetwork !: truckNetowrk;

  private updateOptions(options: string[]) {
    // Clear the existing options
    this.select.innerHTML = '';
    // Add the new options
    for (const option of options) {
      const opt = document.createElement('option');
      opt.innerHTML = option;
      this.select.appendChild(opt);
    }
  }

  private selectedTruck: any;

  private async createScene() {

    let rnService = new RoadNetworkService();
    let warehouses: any[] = [];
    await rnService.getAllWarehouses().then((data) => {
      warehouses = data;
    });
    let paths: any[] = [];
    let limitPerWarehouse = 2;
    for (const warehouse of warehouses) {
      let amountPathOfWarehouse = 0;
      await rnService.getPathBetweenWarehouses(warehouse.id).then((data) => {

        if (data != null) {

          while (amountPathOfWarehouse < limitPerWarehouse) {
            let randomPath = data[Math.floor(Math.random() * data.length)];

            if (!paths.includes(randomPath)) {
              paths.push({ startWHId: randomPath.startWHId, destinationWHId: randomPath.destinationWHId, roadWidth: this.getRandomNumber() });
              amountPathOfWarehouse++;
            }
          }


        }
      });

    }



    let positions = roadNetworkTemplate.calculatePositions(warehouses);
    this.roadNetwork = new roadNetworkTemplate({
      positions: positions,
      paths: paths
    });
    // console.log(this.roadNetwork.maxRoadWidths)
    this.truckNetwork = new truckNetowrk(positions,this.roadNetwork.whAndWidths);

    let whOptions:string[] = [];
    whOptions.push("Select Warehouse");
    this.roadNetwork.whAndWidths.forEach((wh) => {
      whOptions.push(wh.wh);
    });

    this.roadNetwork.object.children.forEach(objectGroup => {
      objectGroup.children.forEach(object => {
        if(object.name==""){
          this.roads.push(object);
        }else if(object.name!="light"){
          this.warehouses.push(object);
        }
      });
       
    });



    this.scene = new THREE.Scene();

    const loader = new THREE.TextureLoader();
    loader.load('assets/sky.jpg', (texture) => {
      this.scene.background = texture;
    });
    
    this.scene.add(this.roadNetwork.object);

    this.scene.add(this.truckNetwork.object);


    this.updateOptions(whOptions);
    
    this.select.addEventListener('change', (_) => {
      let selectedWH = this.select.value;
      if (selectedWH != "Select Warehouse") {
        let whIndex = this.roadNetwork.whAndWidths.findIndex((wh) => wh.wh == selectedWH);
      
        let truckName = this.truckNetwork.object.children[whIndex].name
        
        
        let truck = this.scene.getObjectByName(truckName)?.children[0];
        
        if(truck != undefined){
   
          this.camera.position.set(truck.position.x+0.1,truck.position.y+0.1,truck.position.z+100);
          this.camera.rotation.z = THREE.MathUtils.degToRad(90);
          this.selectedTruck = truck;
          this.player = new Player(this.selectedTruck);
          

        }
      }
      
    });

    
    


    let closestWarehouseDistance = 100000;
    
    this.warehouses.forEach((warehouse) => {
      console.log(warehouse.children)
      // let distance = this.selectedTruck.position.distanceTo(warehouse.position);
      // if(distance < closestWarehouseDistance){
      //   closestWarehouseDistance = distance;
      //   console.log(this.closestWarehouse)
      //   this.closestWarehouse = warehouse;
      // }

    })
   


    //Camera
    this.camera = new THREE.PerspectiveCamera(
      this.fieldOfView,
      this.canvas.clientWidth / this.canvas.clientHeight,
      this.nearClippingPlane,
      this.farClippingPlane
    );

    const listener = new THREE.AudioListener();
    

    // create a global audio source
    const sound = new THREE.Audio(listener);

    // load a sound and set it as the Audio object's buffer
    const audioLoader = new THREE.AudioLoader();
    audioLoader.load('./assets/audio.mp3', function (buffer) {
      sound.setBuffer(buffer);
      sound.setLoop(true);
      sound.setVolume(0.5);
      sound.play();
    });

    // this.camera.add(listener);
    this.camera.position.z = this.cameraZ;


  }

  private animate() {
   
    //get selected truck in table
    let selectedWH = this.select.value;
    if (selectedWH != "Select Warehouse") {
      let whIndex = this.roadNetwork.whAndWidths.findIndex((wh) => wh.wh == selectedWH);
      let truckName = this.truckNetwork.object.children[whIndex].name
      let truck = this.scene.getObjectByName(truckName)?.children[0];
      if(truck != undefined){
        
        this.camera.position.set(truck.position.x+0.1,truck.position.y+0.1,truck.position.z+100);
        this.selectedTruck = truck;
      }
    }
    //get closeste warehouse to truck from warehouses array
    

    // this.checkCollision(this.selectedTruck,this.closestWarehouse);
    
    

  }
 


  private lastwindowWidth: number = window.innerWidth;
  private lastwindowHeight: number = window.innerHeight;
  private onWindowResize() {
    if (this.lastwindowWidth != window.innerWidth || this.lastwindowHeight != window.innerHeight) {
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

    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight);
    let start = true;
    let component: RoadNetworkComponent = this;
    (function render() {
      requestAnimationFrame(render);
      component.animate();
      component.onWindowResize();
      component.renderer.render(component.scene, component.camera);
     if(start){
      component.camera.aspect = window.innerWidth / window.innerHeight;
      component.camera.updateProjectionMatrix();
      component.renderer.setSize(window.innerWidth, window.innerHeight);
       start=false;
     }
    }());

    new OrbitControls(this.camera, this.canvas);
  }


  async ngAfterViewInit() {
    await this.createScene();
    this.startRenderingLoop();
  }


  isAuth: boolean = false;
  authorizedRoles: string[] = ["logMan","admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(!this.authorizedRoles.includes(role)){
      this.router.navigate(['/']);
      return false
    }
    else
      return true;
    
  }

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();

  }


  private getRandomNumber() {
    //return random number between 0.3 and 0.8
    return Math.random() * (0.8 - 0.3) + 0.3;
  }


  private checkCollision(object1: THREE.Object3D,object2: THREE.Object3D) {
    const raycaster = new Raycaster();

    // Set the raycaster's origin and direction based on the positions of the objects
    const origin = object1.position;
    const direction = object2.position.clone().sub(object1.position);
    raycaster.set(origin, direction);
    const material = new THREE.LineBasicMaterial({
      color: 0x0000ff
    });
    
    const points = [];
    points.push( origin );
    points.push( direction );

    
    const geometry = new THREE.BufferGeometry().setFromPoints( points );
    
    const line = new THREE.Line( geometry, material );
    this.scene.add( line );

    // // Check if the ray intersects with the object
    // const intersects = raycaster.intersectObject(testObject);
    // // console.log(intersects)
    // if (intersects.length > 0) {
    //   console.log("Object is colliding with the ray!");
    // }

  }

  

   

}


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