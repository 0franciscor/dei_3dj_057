import { Component, OnInit } from "@angular/core";
import { FormBuilder } from "@angular/forms";
import { Router } from "@angular/router";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { PlanningService } from "src/app/Services/PlanningService/planning-service.service";


@Component({
    selector: 'app-truck-planning-simulation',
    templateUrl: './truck-planning-simulation.component.html',
    styleUrls: ['./truck-planning-simulation.component.css']
})
export class TruckPlanningSimulationComponent implements OnInit {

    truckSimulation = "eTruck01";
    dateSimulation = '2022125';
    public infoList: any []=[]

    constructor(private loginService:LoginService ,private fb: FormBuilder, private router: Router, private planningService:PlanningService) {}

    showPlan={
        truck: "",
        info: []
    }



    isAuth: boolean = false;
    authorizedRoles: string[] = ["logMan", "admin"];
    async isAuthenticated(){
        const role = await this.loginService.getRole();
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
    



    async getBestPath(){
        let answer = await this.planningService.getBestPath(this.truckSimulation,this.dateSimulation)
        this.showPlan = await answer.json();
        this.infoList = this.showPlan.info
    }

    async getHighestMassFirst(){
        let answer = await this.planningService.getHighestMassFirst(this.dateSimulation)
        this.showPlan = await answer.json()
        
    }

    async getClosestWarehouse(){
        let answer = await this.planningService.getClosestWarehouse(this.dateSimulation)
        this.showPlan = await answer.json()
        this.infoList= this.showPlan.info;
    }

    async getCheapestPath(){
        let answer = await this.planningService.getCheapestPath(this.dateSimulation)
        this.showPlan = await answer.json()
        this.infoList = this.showPlan.info
    }

   
    
}