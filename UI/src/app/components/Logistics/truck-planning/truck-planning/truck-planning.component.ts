import { Component, ComponentFactoryResolver, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, FormControl, Validators } from "@angular/forms";
import { PlanningService } from '../../../../Services/PlanningService/planning-service.service';
import { Router } from '@angular/router';

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
  constructor(private fb: FormBuilder, private router: Router, private planningService:PlanningService) { }

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
    console.log(this.showPlan)

  }

  async getHighestMassFirst(){
    let answer = await this.planningService.getHighestMassFirst(this.finaldate)
    console.log(answer)
  }

  async getClosestWarehouse(){
    let answer = await this.planningService.getClosestWarehouse(this.finaldate)
    console.log("123")
  }

  async getCheapestPath(){
    let answer = await this.planningService.getCheapestPath(this.finaldate)
    console.log(answer)
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
}
