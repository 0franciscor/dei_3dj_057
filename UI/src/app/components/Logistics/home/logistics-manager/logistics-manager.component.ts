import { Component, OnInit, ViewChild, NgZone } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { PackagingService } from 'src/app/Services/PackageService/package.service';

import { PathService } from 'src/app/Services/PathService/path.service';

import { MatPaginator } from '@angular/material/paginator';
import { MatTableDataSource } from "@angular/material/table";

interface path{
  startWHId: string;
  destinationWHId: string;
  pathDistance: number;
  pathTravelTime: number;  
  wastedEnergy: number;
  extraTravelTime: number;
};


@Component({
  selector: 'app-logistics-manager',
  templateUrl: './logistics-manager.component.html',
  styleUrls: ['./logistics-manager.component.css']
})
export class LogisticsManagerComponent implements OnInit {

  public selectedPathOption : path[]=[];
  public selectedPath: any;

  public pathList: any[]=[];

   dataSource!: MatTableDataSource<path>; 

  @ViewChild('paginator',{static:false})
  set paginator(value:MatPaginator){
    if(this.dataSource){
      this.dataSource.paginator=value;
    }
  } 

  displayedColumns: String[]=['startWHId', 'destinationWHId', 'pathDistance', 'pathTravelTime', 'wastedEnergy', 'extraTravelTime']

  formSelectWarehouse!: FormGroup;
  constructor(private ngZone:NgZone,private loginService:LoginService,private router: Router,private pathService: PathService,private fb: FormBuilder,private packageService: PackagingService) {
    this.selectedPath={
      pathID:"",
      startWHId: undefined,
      destinationWHId: undefined,
      pathDistance: undefined,
      pathTravelTime: undefined,
      wastedEnergy: undefined,
      extraTravelTime: undefined,
    }
  }

  isAuth: boolean = false;
  authorizedRoles: string[] = ["logMan","admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(!this.authorizedRoles.includes(role)){
      this.ngZone.run(() => this.router.navigate(['/']));
      return false
    }
    else
      return true;
    
  }

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();
    this.formSelectWarehouse = this.fb.group({
      startWHId: [''],
      destinationWHId:['']
    });
  }

  onPathSelected($event: any) {
    let test = this.pathList.find(element => element.pathID == this.selectedPathOption);
    this.selectedPath = test;
  }

  onStartWarehouseSelected($event:any){
    let test = this.pathList.find(element => element.startWHId == this.selectedPathOption);
    this.selectedPath = test;
  }

  onDestinationWarehouseSelected($event: any){
    let test= this.pathList.find(element => element.destinationWHId == this.selectedPathOption);
    this.selectedPath = test;
  }

  onSubmit(){
    this.selectedPathOption=[];
    let test;
    if(this.formSelectWarehouse.value.startWHId == undefined){
      test= this.pathList.find(element => element.destinationWHId == this.selectedPathOption)
    }else if(this.formSelectWarehouse.value.destinationWHId == undefined){
      test= this.pathList.find(element => element.startWHId == this.selectedPathOption)
    }else{
       test = this.pathList.find(element=>element.startWHId == this.selectedPathOption && element.destinationWHId == this.selectedPathOption)
    }
   
    this.pathService.getAllPaths(this.formSelectWarehouse.value).then((data)=>{
      for(let i=0;i<data.length;i++){
        this.selectedPathOption[i]= data[i];
        this.dataSource = new MatTableDataSource(this.selectedPathOption);
      }
      
      this.selectedPathOption = data;
      
      });
    
    
  }


  goToCreatePath(){
    this.ngZone.run(() => this.router.navigate(['/Logistics/Path/CreatePath']));
  }

  goToCreatePackage(){
    this.ngZone.run(() => this.router.navigate(['/Logistics/Packaging/CreatePackage']));
  }

  goToRoadNetwork() {
    this.ngZone.run(() => this.router.navigate(['/Logistics/RoadNetwork']));
  }

  goToTruckPlanning(){
    this.ngZone.run(() => this.router.navigate(['/Logistics/TruckPlanning']));
  }

  goToPackageList() {
    this.ngZone.run(() => this.router.navigate(['/Logistics/Packaging/ListPackage']));
  }

}
