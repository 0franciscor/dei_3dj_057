import { Component, ComponentFactoryResolver, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, FormControl, Validators } from "@angular/forms";
import { PlanningService } from '../../../../Services/PlanningService/planning-service.service';
import { Router } from '@angular/router';
import { TripService } from 'src/app/Services/TripService/trip.service';

@Component({
  selector: 'app-truck-planning',
  templateUrl: './truck-planning.component.html',
  styleUrls: ['./truck-planning.component.css']
})
export class TruckPlanningComponent implements OnInit {

  public selectedDate: any;
  public  finaldate :any;
  formPlanning!: FormGroup;

  public infoList: any []=[]


  public truck : any;
  constructor(private fb: FormBuilder, private router: Router, private planningService:PlanningService, private tripService:TripService) { }

  selectedPlan={
    truckName: "",
    planDate: undefined
  }

  showPlan={
   truck:"",
   info:[]
  }

  ngOnInit(): void {
    this.formPlanning = this.fb.group({
      truckName: new FormControl('',[Validators.required]),
      planDate: new FormControl('',[Validators.required]),
    });
  }


 onSubmit(){
  let yourDate=this.formatDate(this.formPlanning.value.planDate)
  let datesplit=(yourDate).split("/");

  if(datesplit[2].charAt(0)=="0"){
    datesplit[2]=datesplit[2].charAt(1)
  }
  
  this.finaldate = datesplit[0]+datesplit[1]+datesplit[2];
  this.formPlanning.value.truckName= "eTruck01"
 }


  async getBestPath(){
    this.onSubmit();
    let answer = await this.planningService.getBestPath(this.formPlanning.value.truckName, this.finaldate)
    this.showPlan=await answer.json()
    this.infoList= this.showPlan.info
    

  }

  async getHighestMassFirst(){
    this.onSubmit();
    let answer = await this.planningService.getHighestMassFirst(this.finaldate)
    this.showPlan=await answer.json()
    this.infoList= this.showPlan.info
    
  }

  async getClosestWarehouse(){
    this.onSubmit();
    let answer = await this.planningService.getClosestWarehouse(this.finaldate)
    this.showPlan=await answer.json()
    this.infoList= this.showPlan.info
    
  }

  async getCheapestPath(){
    this.onSubmit();
    let answer = await this.planningService.getCheapestPath(this.finaldate)
    this.showPlan=await answer.json()
    this.infoList= this.showPlan.info
    
  }

   formatDate(date: Date) {
    var d = new Date(date),
        month = '' + (d.getMonth() + 1),
        day = '' + d.getDate(),
        year = d.getFullYear();

    if (month.length < 2) 
        month = '0' + month;
    if (day.length < 2) 
        day = '0' + day;

    return [year, month, day].join('/');
  }
  savePlan(){
    
    const savePlan = {
      truckName: this.formPlanning.value.truckName,
      planDate: this.finaldate,
      infoList: this.infoList
    }
    
    
    console.log(savePlan)
    this.tripService.createTrip(savePlan);
    
  }
}
