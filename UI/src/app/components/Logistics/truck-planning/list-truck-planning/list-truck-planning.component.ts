import { Component, OnInit, ViewChild, AfterViewInit, NgZone } from "@angular/core";
import { MatSort } from '@angular/material/sort';
import { MatPaginator } from "@angular/material/paginator";
import { MatTableDataSource } from "@angular/material/table";
import { Router } from "@angular/router";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { TripService } from "src/app/Services/TripService/trip.service";
import { MatSelectChange } from "@angular/material/select";


interface Trip {
    tripID: string;
    date: string;
    pathIDlist: string[];
    truckID: string;
    deliveryIDlist: string[];
}
interface Option{
    id: string;
    choice: string;
}

@Component({
    selector: 'app-list-truck-planning',
    templateUrl: './list-truck-planning.component.html',
    styleUrls: ['./list-truck-planning.component.css']
})


export class ListTruckPlanningComponent implements OnInit {

    public tripList: any[] = [];
    public originalPackageList: any[] = [];

    displayedColumns: string[] = ['tripID', 'date', 'pathIDlist', 'truckID', 'deliveryIDlist'];

    options: Option[] = [
        {id: 'tripID', choice: 'Trip ID'},
        {id: 'date', choice: 'Date'},
        {id: 'truckID', choice: 'Truck ID'},
    ];

    selectedOption!: string;

    constructor(private ngZone:NgZone,private loginService: LoginService, private tripService: TripService, private router: Router) {}

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
            this.ngZone.run(() => this.router.navigate(['/']));
            return false;
        }
        return true;
    }

    loadTrips() {
        this.tripService.getAllTrips().then((data) => {
            this.tripList = data;
            this.originalPackageList = this.tripList.slice();
            this.dataSource = new MatTableDataSource(this.tripList);
        });
    }

    async ngOnInit() {
        this.isAuth = await this.isAuthenticated();
        if (this.isAuth) {
            this.loadTrips();
            
        }
    }

    filterSearch(event: Event) {
        const filvalue = (event.target as HTMLInputElement).value;
        this.dataSource.filter = filvalue;
    }

    changeOption(event: MatSelectChange) {
        this.selectedOption = event.value;
        if(this.selectedOption == 'tripID'){
            this.dataSource.filterPredicate = function(data: Trip, filter: string): boolean {
                return data.tripID.toLowerCase().includes(filter);
            };
        }
        else if(this.selectedOption == 'date'){
            this.dataSource.filterPredicate = function(data: Trip, filter: string): boolean{
                return data.date.toLowerCase().includes(filter);
            };
        }
        else if(this.selectedOption == 'truckID'){
            this.dataSource.filterPredicate = function(data: Trip, filter: string): boolean{
                return data.truckID.toLowerCase().includes(filter);
            };
        }
    }
}
