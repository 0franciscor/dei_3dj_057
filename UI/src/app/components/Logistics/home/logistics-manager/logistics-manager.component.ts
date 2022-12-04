import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Router } from '@angular/router';
import { elementAt } from 'rxjs';

import{ PathService } from 'src/app/Services/PathService/path.service';

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

  formSelectWarehouse!: FormGroup;
  constructor(private router: Router,private pathService: PathService,private fb: FormBuilder) {
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


  ngOnInit(): void {
    this.formSelectWarehouse = this.fb.group({
      startWHId: [''],
      destinationWHId:['']
    });
  }

  onPathSelected($event: any) {
    let test = this.pathList.find(element => element.pathID == this.selectedPathOption);
    console.log(test);
    this.selectedPath = test;
  }

  onStartWarehouseSelected($event:any){
    let test = this.pathList.find(element => element.startWHId == this.selectedPathOption);
    console.log(test);
    this.selectedPath = test;
  }

  onDestinationWarehouseSelected($event: any){
    let test= this.pathList.find(element => element.destinationWHId == this.selectedPathOption);
    console.log(test);
    this.selectedPath = test;
  }

  onSubmit(){
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
      }
      
      console.log(this.selectedPathOption)
      this.selectedPathOption = data;
      });
    
    
  }

  goToCreatePath(){
    this.router.navigate(['/Logistics/Path/CreatePath']);
  }

  goToRoadNetwork() {
    this.router.navigate(['/Logistics/RoadNetwork']);
  }

  goToTruckPlanning(){
    this.router.navigate(['/Logistics/TruckPlanning']);
  }

}
