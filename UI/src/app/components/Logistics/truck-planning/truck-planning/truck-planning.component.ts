import { Component, OnInit, NgZone } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from "@angular/forms";
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { TripService } from 'src/app/Services/TripService/trip.service';
import { PlanningService } from '../../../../Services/PlanningService/planning-service.service';

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
  public isGenetic: boolean = false;

  public truck : any;

  constructor(private ngZone:NgZone,private loginService:LoginService ,private fb: FormBuilder, private router: Router, private planningService:PlanningService, private tripService:TripService) { }

  selectedPlan={
    truckName: "",
    planDate: undefined
  }

  showPlan={
   truck:"",
   info:[]
  }

  info={
    truck1:"",
    bestRoute1:"",
    truck2:"",
    bestRoute2:"",
    truck3:"",
    bestRoute3:"",
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
    
    this.formPlanning = this.fb.group({
      truckName: new FormControl('',[Validators.required]),
      planDate: new FormControl('',[Validators.required]),
    });
  }


 onSubmit(){
  let yourDate=this.formatDate(this.formPlanning.value.planDate);
  let datesplit=(yourDate).split("/");

  if(datesplit[1].charAt(0)=="0"){
    datesplit[1]=datesplit[1].charAt(1)
  }

  if(datesplit[2].charAt(0)=="0"){
    datesplit[2]=datesplit[2].charAt(1)
  }
  
  this.finaldate = datesplit[0]+datesplit[1]+datesplit[2];
  
  
 }


  async getBestPath(){
    this.isGenetic=false;
    this.formPlanning.controls['truckName'].setValue("eTruck01");
    if(this.formPlanning.valid){
      this.onSubmit();
      
      let answer = await this.planningService.getBestPath(this.formPlanning.value.truckName, this.finaldate)
      this.showPlan=await answer.json()
      this.infoList= this.showPlan.info
    }
    
    

  }

  async getHighestMassFirst(){
    this.isGenetic=false;
    this.onSubmit();
    let answer = await this.planningService.getHighestMassFirst(this.finaldate)
    this.showPlan=await answer.json()
    this.infoList= this.showPlan.info
    
  }

  async getClosestWarehouse(){
    this.isGenetic=false;
    this.onSubmit();
    let answer = await this.planningService.getClosestWarehouse(this.finaldate)
    this.showPlan=await answer.json()
    this.infoList= this.showPlan.info
    
  }

  async getCheapestPath(){
    this.isGenetic=false;
    this.onSubmit();
    let answer = await this.planningService.getCheapestPath(this.finaldate)
    this.showPlan=await answer.json()
    this.infoList= this.showPlan.info
    
  }

  async getGeneticAlgorithm(){
    this.onSubmit();
    let answer = await this.planningService.getGeneticAlgorithm(this.finaldate)
    this.info = answer
    this.isGenetic=true;

    
  }

  async getTruckPlanningSimulation(){
    this.ngZone.run(() => this.router.navigate(['Logistics/TruckPlanning/TruckPlanningSimulation']));
    
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
 
    this.tripService.createTrip(savePlan);
    
  }

  goToListTruckPlanning(){
    this.ngZone.run(() => this.router.navigate(['Logistics/TruckPlanning/ListTruckPlanning']));
  }



}
