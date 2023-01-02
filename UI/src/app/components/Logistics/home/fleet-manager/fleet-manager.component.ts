import { Component, Inject, OnInit } from '@angular/core';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { TruckService } from 'src/app/Services/TruckService/truck.service';



interface Truck{
  truckID: string;
  tare: number;
  capacity: number;
  maxBatteryCapacity: number;
  autonomy: number;
  fastChargeTime: number;
}

export interface DialogData {
  name: string;
  message: string;
}

@Component({
  selector: 'app-fleet-manager',
  templateUrl: './fleet-manager.component.html',
  styleUrls: ['./fleet-manager.component.css']
})
export class FleetManagerComponent implements OnInit {

  public selectedTruckOption : any;
  public selectedTruck: any;
  
  public truckList: any[] = [];

  public seeAll: boolean = false;

  displayedColumns: string[] = ['TruckID', 'Tare', 'Capacity', 'Maximum Battery Capacity', 'Autonomy', 'Fast Charge Time', "Actions"];
  dataSource = this.truckList;
  
  constructor(private loginService:LoginService,public dialog: MatDialog,private truckService: TruckService, private router: Router) {}

  isAuth: boolean = false;
  isAdmin: boolean = false;
  authorizedRoles: string[] = ["fltMan","admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(role == "admin")
      this.isAdmin = true;
    if(!this.authorizedRoles.includes(role)){
      this.router.navigate(['/']);
      return false
    }
    else
      return true;
    
  }

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();
    if(this.isAuth){
      this.truckService.getAllTruck().then((data) => {
        this.truckList = data;
        this.dataSource = this.truckList;
      });
      this.selectedTruck= {
        truckID: "",
        tare: undefined,
        capacity: undefined,
        maxBatteryCapacity: undefined,
        autonomy: undefined,
        fastChargeTime: undefined,
        active: undefined
      }
    }
  }

  onTruckSelected($event: any){
    this.seeAll=false;
    this.selectedTruck = this.truckList.find(element => element.truckID == this.selectedTruckOption);;
    
  }


  goToCreateTruck() {
    this.router.navigate(['Logistics/Truck/CreateTruck']);
    
  }
  goToEditTruck(truckID:string) {
    this.router.navigate(['Logistics/Truck/EditTruck', truckID]);
    
  } 
  async toggleActiveTruck(truckID:string) {
    
    let truck = this.truckList.find(element => element.truckID == truckID);
    let answer = await this.truckService.toggleActiveTruck(truckID);
    let message = "";
    if(truck.active)
      message = "Truck Disabled Successfully";
    else
      message = "Truck Enabled Successfully";
    if(answer.status != 200){
      message = "Truck Deletion Failed";
    }
    const dialogRef = this.dialog.open(DeleteTruckComponentDialog, {
      width: '250px',
      data: {
        name: truckID,
        message: message},
    });

    dialogRef.afterClosed().subscribe(result => {
      if(answer.status == 200)
        this.router.navigate(['Logistics/Home/FleetManager']);
      
    });
    this.truckService.getAllTruck().then((data) => {
      this.truckList = data;
      this.dataSource = this.truckList;
    });
    if(this.selectedTruckOption!="")
      this.selectedTruck.active = !this.selectedTruck.active;
    
  }

  toggleSeeAll(){
    this.selectedTruckOption="";
    this.seeAll=!this.seeAll;
  }

  async deleteTruck(truckID:string) {
    if(!this.selectedTruckOption.active){
      let answer = await this.truckService.deleteTruck(truckID);
      let message = "Truck Deleted Successfully";
      
        
      if(answer.status != 200){
        message = "Truck Deletion Failed";
      }
      const dialogRef = this.dialog.open(DeleteTruckComponentDialog, {
        width: '250px',
        data: {
          name: truckID,
          message: message},
      });
  
      dialogRef.afterClosed().subscribe(result => {
        if(answer.status == 200)
          this.router.navigate(['Logistics/Home/FleetManager']);
        
      });
      this.truckService.getAllTruck().then((data) => {
        this.truckList = data;
        this.dataSource = this.truckList;
      });

      if(this.selectedTruckOption!="")
        window.location.reload();

    }
  }
    

}

@Component({
  selector: 'app-fleet-manager',
  templateUrl: 'delete-truck-dialog.html',
})
export class DeleteTruckComponentDialog {
  constructor(
    public dialogRef: MatDialogRef<DeleteTruckComponentDialog>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData,
  ) {}

  ngOnInit():void{}

  onOk(): void {
    this.dialogRef.close();
  }
}
