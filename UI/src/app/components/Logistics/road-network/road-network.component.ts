import { AfterViewInit, Component, ElementRef, Input, OnInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { random } from 'cypress/types/lodash';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { RoadNetworkService } from 'src/app/Services/RoadNetworkService/road-network.service';
import * as THREE from 'three';
import { Object3D, Raycaster, VSMShadowMap } from 'three';
import { PCFShadowMap, PCFSoftShadowMap } from 'three';
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";
import Player from './RoadNetworkJS/player';
import roadNetworkTemplate from './RoadNetworkJS/road-network';
import TruckNetwork from './RoadNetworkJS/truck-network';


interface Warehouse {
  id: string;
  address: string;
  altitude: number;
  latitude: string;
  longitude: string;
  designation: string;
  city: string;
  active: boolean;
}

interface Truck {
  id: string;
  truckID: string;
  tare: number;
  capacity: number;
  maxBatteryCapacity: number;
  autonomy: number;
  fastChargeTime: number;
  active: boolean;
}
  



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
  
  private controls!: OrbitControls;

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

  private rnService = new RoadNetworkService();

  private player!: Player;
  private playerPositionObject!: Object3D;
  private roads: Object3D[] = [];
  private warehouses: Object3D[] = [];

  private renderer!: THREE.WebGLRenderer;

  private scene!: THREE.Scene;

  private roadNetwork !: roadNetworkTemplate;
  private truckNetwork !: TruckNetwork;

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

  private selectedTruck?: Object3D;


  private maxIncline = 25;

  private calculateIncline(path:any,warehouses:Warehouse[],roadWidth:number){
    let positions = roadNetworkTemplate.calculatePositions(warehouses);
    let startWH: any;
    let destinationWH:any;
    positions.forEach((position) => {

      if(position.wh==path.startWHId){
        startWH = position;
      }
      if(position.wh==path.destinationWHId){
        destinationWH = position;
      }
    });
    const connectionConstant = 1;
    const circleConstant = 2;
    let connectionLength = connectionConstant * circleConstant;
    if(startWH && destinationWH){
      let angle = Math.sqrt(Math.pow((destinationWH.x - startWH.x), 2) + Math.pow((destinationWH.y - startWH.y), 2)) - connectionLength * 2;
    
      let incline = Math.abs(Math.atan2((destinationWH.z - startWH.z), angle))
      let inclinePercentage = incline * 100;
      if(inclinePercentage <= this.maxIncline){
        return true
      }
    }
    return false;
  }

  private async createWarehouses(): Promise<Warehouse[]> {
    let allWarehouseList = await this.rnService.getAllWarehouses()
    let activeWarehouseList: Warehouse[] = [];
    allWarehouseList.forEach((warehouse:Warehouse) => {
      if(warehouse.active){
        activeWarehouseList.push(warehouse);
      }
    });

    return activeWarehouseList;

  }

  private async createTrucks(maxAmount:number) {
    let allTruckList = await this.rnService.getAllTrucks()
    let activeTruckList: Truck[] = [];
    let truckList: Truck[] = [];
    allTruckList.forEach((truck:Truck) => {
      if(truck.active){
        activeTruckList.push(truck);
      }
    });
    if(activeTruckList.length>maxAmount){
      //take random trucks without duplicates
      let randomNumList=[];
      let randomPos = Math.floor(Math.random() * activeTruckList.length)
      let randomTruck = activeTruckList[randomPos];
      randomNumList.push(randomPos);
      truckList.push(randomTruck);
      while(truckList.length<maxAmount){
        randomPos = Math.floor(Math.random() * activeTruckList.length)
        if(!randomNumList.includes(randomPos)){
          randomTruck = activeTruckList[randomPos];
          randomNumList.push(randomPos);
          truckList.push(randomTruck);
        }
      }
    }
    else
      truckList = activeTruckList;
    return truckList;
  }

  private populatePaths(limitPerWarehouse:number,warehouses:Warehouse[],data:any[]){
    let paths: any[] = [];
    let amountPathOfWarehouse = 0;
    while (amountPathOfWarehouse < limitPerWarehouse) {
      let randomNumList=[];
      let randomPos = Math.floor(Math.random() * data.length)
      let randomPath = data[randomPos];
      randomNumList.push(randomPos);
      
      let thisRoadWidth = this.getRandomNumber();
      let legalIncline = false; 
      while(!legalIncline && randomNumList.length<=data.length){

        randomPos = Math.floor(Math.random() * data.length)
        if(!randomNumList.includes(randomPos)){
          randomPath = data[randomPos];
          randomNumList.push(randomPath);
          
          legalIncline=this.calculateIncline(randomPath,warehouses,thisRoadWidth);
        }
      }
      if(!legalIncline){
        randomPath = data[Math.floor(Math.random() * data.length)];
      }
      if (!paths.includes(randomPath)) {
        
        paths.push({ startWHId: randomPath.startWHId, destinationWHId: randomPath.destinationWHId, roadWidth: thisRoadWidth });
        amountPathOfWarehouse++;
      }
    }
    return paths;
  }

  private async createPaths(warehouses: Warehouse[]) {
    let paths: any[] = [];
    let limitPerWarehouse = 2;
    for (const warehouse of warehouses) {
      if(warehouse.active){
        await this.rnService.getPathBetweenWarehouses(warehouse.id).then((data) => {
          if (data != null)
            paths = paths.concat(this.populatePaths(limitPerWarehouse,warehouses,data));
        });
      }
    }
    return paths;
  }



  private async createScene() {

   
    let warehouses: Warehouse[] = await this.createWarehouses();
    
    let paths: any[] = await this.createPaths(warehouses);

    let positions = roadNetworkTemplate.calculatePositions(warehouses);
    this.roadNetwork = new roadNetworkTemplate({
      positions: positions,
      paths: paths
    });
    let truckObjects = await this.createTrucks(warehouses.length);
    this.truckNetwork = new TruckNetwork(positions,this.roadNetwork.whAndWidths, truckObjects);

    let truckOption:string[] = [];
    truckOption.push("Select Truck");
    this.truckNetwork.truckNames.forEach((truck) => {
      truckOption.push(truck);
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


    this.updateOptions(truckOption);
    
    this.playerPositionObject = new THREE.Object3D();

    this.select.addEventListener('change', (_) => {
      let selectedTruck = this.select.value;
      if (selectedTruck != "Select Truck") {
        let whIndex = this.truckNetwork.truckNames.findIndex((truck) => truck == selectedTruck);
      
        let truckName = this.truckNetwork.object.children[whIndex].name
        
        
        let truck = this.scene.getObjectByName(truckName)?.children[0];
        
        if(truck != undefined){
          this.selectedTruck = truck;
          this.lastPosition = this.selectedTruck.position;
          this.camera.position.z = this.selectedTruck.position.z + 10;
          if(this.player != undefined)
            this.player.destroy();
          this.player = new Player(this.selectedTruck);
          this.controls.target.copy(this.selectedTruck.position);

          //create a cube
          let cubeGeometry = new THREE.BoxGeometry(0.5, 0.5, 0.5);
          let cubeMaterial = new THREE.MeshBasicMaterial({ color: 0x00ff00 });
          let cube = new THREE.Mesh(cubeGeometry, cubeMaterial);
          cube.position.set(this.selectedTruck.position.x, this.selectedTruck.position.y, this.selectedTruck.position.z+2);
          
          //removes all objects from playerPositionObject
          if(this.playerPositionObject.children.length>0)
          this.playerPositionObject.children.forEach((object) => {
            this.playerPositionObject.remove(object);
          });
          this.playerPositionObject.add(cube);
          

          

        }
      }else{
        if(this.playerPositionObject.children.length>0)
          this.playerPositionObject.children.forEach((object) => {
            this.playerPositionObject.remove(object);
          });
        this.player.destroy();
        this.selectedTruck = undefined;
        
      }
      
    });

    
    this.scene.add(this.playerPositionObject);


    
  
    const ambientLight = new THREE.AmbientLight(0xFFFFFF,0.3);
    ambientLight.position.set(-100,100,100);
    const directionalLight= new THREE.DirectionalLight(0xFFFFFF,1);
    directionalLight.position.set(-100,100,100);
    
    directionalLight.castShadow=true;
    directionalLight.shadow.mapSize.width = 4096
    directionalLight.shadow.mapSize.height = 4096
    directionalLight.shadow.camera.near = 1;
    directionalLight.shadow.camera.far = 500.0;
    directionalLight.shadow.camera.left = -100;
    directionalLight.shadow.camera.right = 100;
    directionalLight.shadow.camera.top = 100;
    directionalLight.shadow.camera.bottom = -100;
    
    //this.scene.add(new THREE.CameraHelper(directionalLight.shadow.camera))
    this.scene.add(directionalLight);
    this.scene.add(ambientLight);
    
    for(let i=0; i<warehouses.length; i++){
      this.warehouses[i].castShadow = true;
    }
    
    //Camera
    this.camera = new THREE.PerspectiveCamera(
      this.fieldOfView,
      this.canvas.clientWidth / this.canvas.clientHeight,
      this.nearClippingPlane,
      this.farClippingPlane
    );
    this.camera.up.set(0, 0, 1);

    this.camera.position.z = this.cameraZ;
    
        
    

    

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


  }

  private lastPosition = new THREE.Vector3();

  private animate() {
   
    if(this.selectedTruck != undefined){
      this.controls.update();
        
      this.playerPositionObject.children[0].position.set(this.selectedTruck.position.x, this.selectedTruck.position.y, this.selectedTruck.position.z+2);
      this.camera.position.lerp(this.selectedTruck.position, 0);
      this.camera.lookAt(this.selectedTruck.position);
      this.renderer.render(this.scene, this.camera);

    }
    // let selectedTruck = this.select.value;
    // if (selectedTruck != "Select Truck") {
    //   let whIndex = this.truckNetwork.truckNames.findIndex((truck) => truck == selectedTruck);
    
    //   let truckName = this.truckNetwork.object.children[whIndex].name
    //   let truck = this.scene.getObjectByName(truckName)?.children[0];
      
    //   if(truck != undefined){
    //     console.log(this.selectedTruck)
        
    //     this.selectedTruck = truck;
        
        
        
        
        
          

    //   }
    // }
   
    
    

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
    this.renderer.shadowMap.enabled = true;
    
    this.renderer.shadowMap.type = PCFSoftShadowMap;
    this.renderer.setSize(this.canvas.clientWidth, this.canvas.clientHeight);
    let start = true;
    //ENABLE SHADOWS
   

    


    let component: RoadNetworkComponent = this;
    (function render() {
      requestAnimationFrame(render);
      component.gravity();
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

    this.controls = new OrbitControls(this.camera, this.canvas);
    this.controls.target.set(0, 0, 0);
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


  private gravity() {
    if(this.selectedTruck){
      // First, create a new raycaster object
      let raycaster = new THREE.Raycaster();
    
      // Next, set the origin and direction of the ray to match the position and direction of the truck's fall
      let raycastPosition = new THREE.Vector3(this.selectedTruck.position.x, this.selectedTruck.position.y, this.selectedTruck.position.z+0.5);
      raycaster.set(raycastPosition, new THREE.Vector3(0, 0, -1)); // Direction of fall is (0, 0, -1)
      raycaster.far = 1;
      // Use the raycaster to check for intersections between the ray and the list of roads
      let intersects = raycaster.intersectObjects(this.roads);


      
      if(intersects.length != 0){
        // Get the intersection point and calculate the distance between it and the bottom of the truck
        let intersection = intersects[0];
        let distance = intersection.point.z - this.selectedTruck.position.z;
        // Adjust the position of the truck to keep it touching the road
        this.selectedTruck.position.z += distance+0.01;

      }

      raycastPosition = this.player.getNextForwardPosition();
      raycastPosition.z += 0.5;
      raycaster.set(raycastPosition, new THREE.Vector3(0, 0, -1));
      intersects = raycaster.intersectObjects(this.roads);
      if (intersects.length === 0)
        this.player.changeMoveForward(false);
      else
        this.player.changeMoveForward(true);
      raycastPosition = this.player.getNextBackwardPosition();
      raycastPosition.z += 0.5;
      raycaster.set(raycastPosition, new THREE.Vector3(0,0,-1))
      intersects = raycaster.intersectObjects(this.roads);
      if(intersects.length === 0)
        this.player.changeMoveBackward(false);
      else
        this.player.changeMoveBackward(true);

      raycastPosition = this.player.getNextForwardPosition();
      raycastPosition.z += 0.5;
      raycaster.set(raycastPosition, new THREE.Vector3(0, 0, -1));
      intersects = raycaster.intersectObjects(this.warehouses);
      if (intersects.length != 0 && this.player.getMoveForward())
        this.player.changeMoveForward(false);
      
      raycastPosition = this.player.getNextBackwardPosition();
      raycastPosition.z += 0.5;
      raycaster.set(raycastPosition, new THREE.Vector3(0, 0, -1));
      intersects = raycaster.intersectObjects(this.warehouses);
      if (intersects.length != 0 && this.player.getMoveBackward())
        this.player.changeMoveBackward(false);
      
        

    }

  }
  


  

   

}


