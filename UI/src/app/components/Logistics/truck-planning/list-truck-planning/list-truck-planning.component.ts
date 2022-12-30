import {LiveAnnouncer} from '@angular/cdk/a11y';
import { Component, OnInit, ViewChild, AfterViewInit } from "@angular/core";
import { MatSort } from '@angular/material/sort';
import { MatPaginator } from "@angular/material/paginator";
import { MatTableDataSource } from "@angular/material/table";
import { Router } from "@angular/router";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { TripService } from "src/app/Services/TripService/trip.service";

@Component({
    selector: 'app-list-truck-planning',
    templateUrl: './list-truck-planning.component.html',
    styleUrls: ['./list-truck-planning.component.css']
})

export class ListTruckPlanningComponent implements OnInit {

    public tripList: any[] = [];
    public originalPackageList: any[] = [];

    displayedColumns: string[] = ['tripID', 'date', 'pathIDlist', 'truckID', 'deliveryIDlist'];

    constructor(private loginService: LoginService, private tripService: TripService, private router: Router) {}

    dataSource!: MatTableDataSource<any>;

    @ViewChild('paginator', { static: false })
    set paginator(value: MatPaginator) {
        if (this.dataSource){
            this.dataSource.paginator = value;
            this.dataSource.sort = this.sort;
        }

    }

    @ViewChild(MatSort) sort!: MatSort;

    isAuth: Boolean = false;
    authorizedRoles: string[] = ["logMan", "admin"];
    async isAuthenticated() {
        const role = await this.loginService.getRole();
        if (!this.authorizedRoles.includes(role)) {
            this.router.navigate(['/']);
            return false
        }
        return true;
    }

    loadTrips() {
        this.tripList = [
            { tripID: "1", date: "2021-05-01", pathIDlist: ["1", "2"], truckID: "1", deliveryIDlist: ["1", "2"] },
            { tripID: "2", date: "2021-05-02", pathIDlist: ["1", "2"], truckID: "2", deliveryIDlist: ["1", "2"] },
            { tripID: "3", date: "2021-05-03", pathIDlist: ["1", "2"], truckID: "3", deliveryIDlist: ["1", "2"] },
            { tripID: "4", date: "2021-05-04", pathIDlist: ["1", "2"], truckID: "4", deliveryIDlist: ["1", "2"] },
            { tripID: "5", date: "2021-05-05", pathIDlist: ["1", "2"], truckID: "5", deliveryIDlist: ["1", "2"] },
            { tripID: "6", date: "2021-05-06", pathIDlist: ["1", "2"], truckID: "6", deliveryIDlist: ["3", "4"] },
        ]

        this.dataSource =  new MatTableDataSource(this.tripList);
    }

    async ngOnInit() {
        this.isAuth = await this.isAuthenticated();
        if (this.isAuth) {
            this.originalPackageList = this.tripList.slice();
            this.loadTrips();
        }
    }
}
