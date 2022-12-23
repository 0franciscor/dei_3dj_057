
import { Component, OnInit, ViewChild } from "@angular/core";

import { MatPaginator } from "@angular/material/paginator";
import { MatTableDataSource } from "@angular/material/table";
import { Router } from "@angular/router";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { TripService } from "src/app/Services/TripService/trip.service";

interface Trip {
    tripID: string
    date: string;
    pathIDlist: string[];
    truckID: string;
    deliveryIDlist: string[];
}

@Component({
    selector: 'app-list-truck-planning',
    templateUrl:'./list-truck-planning.component.html',
    styleUrls: ['./list-truck-planning.component.css']
})

export class ListTruckPlanningComponent implements OnInit{
    
    constructor(private loginService:LoginService,private tripService: TripService,private router: Router){  }

    public tripList: Trip[] = [];
    public originalPackageList: Trip[] = [];
  

    displayedColumns: string[] = ['tripID','date','pathIDlist','truckID','deliveryIDlist'];

    dataSource!: MatTableDataSource<Trip>;

    @ViewChild('paginator',{static: false})
    set paginator(value: MatPaginator){
        if(this.dataSource){
            this.dataSource.paginator = value
        }
    }

    isAuth: Boolean = false;
    authorizedRoles: string[] = ["logMan","admin"];
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
        if(this.isAuth)
        this.tripService.getAllTrips().then((data) => {
            this.tripList = data;
            this.originalPackageList = data.slice();
            console.log(this.originalPackageList);
            this.dataSource = new MatTableDataSource(this.tripList);
        }) ;
    }

}