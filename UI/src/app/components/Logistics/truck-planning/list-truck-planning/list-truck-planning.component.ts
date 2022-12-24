
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
    templateUrl: './list-truck-planning.component.html',
    styleUrls: ['./list-truck-planning.component.css']
})

export class ListTruckPlanningComponent implements OnInit {

    constructor(private loginService: LoginService, private tripService: TripService, private router: Router) {
        this.loadTrips();
    }

    public tripList: Trip[] = [];
    public originalPackageList: Trip[] = [];


    displayedColumns: string[] = ['tripID', 'date', 'pathIDlist', 'truckID', 'deliveryIDlist'];

    dataSource!: MatTableDataSource<Trip>;

    @ViewChild('paginator', { static: true })
    set paginator(value: MatPaginator) {
        if (this.dataSource) {
            this.dataSource.paginator = value;
        }
    }

    isAuth: Boolean = false;
    authorizedRoles: string[] = ["logMan", "admin"];
    async isAuthenticated() {
        const role = await this.loginService.getRole();
        if (!this.authorizedRoles.includes(role)) {
            this.router.navigate(['/']);
            return false
        }
        else
            return true;
    }


    loadTrips() {
        this.tripList = [
            { tripID: "1", date: "2021-05-01", pathIDlist: ["1", "2"], truckID: "1", deliveryIDlist: ["1", "2"] },
            { tripID: "2", date: "2021-05-02", pathIDlist: ["1", "2"], truckID: "1", deliveryIDlist: ["1", "2"] },
            { tripID: "3", date: "2021-05-03", pathIDlist: ["1", "2"], truckID: "1", deliveryIDlist: ["1", "2"] },
            { tripID: "4", date: "2021-05-04", pathIDlist: ["1", "2"], truckID: "1", deliveryIDlist: ["1", "2"] },
            { tripID: "5", date: "2021-05-05", pathIDlist: ["1", "2"], truckID: "1", deliveryIDlist: ["1", "2"] },
            { tripID: "6", date: "2021-05-06", pathIDlist: ["1", "2"], truckID: "1", deliveryIDlist: ["1", "2"] },
        ]

        this.dataSource =  new MatTableDataSource(this.tripList);
    }

    async ngOnInit() {
        this.isAuth = await this.isAuthenticated();
        if (this.isAuth) 
            this.tripService.getAllTrips().then((data) => {
                this.tripList = data;
                this.originalPackageList = data.slice();
                this.loadTrips();
            });
    }
}
