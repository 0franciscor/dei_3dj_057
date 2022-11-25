import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { TruckService } from 'src/app/Services/TruckService/truck.service';


interface Truck{
  truckID: string;
  tare: number;
  capacity: number;
  maxBatteryCapacity: number;
  autonomy: number;
  fastChargeTime: number;
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
  
  constructor(private truckService: TruckService, private router: Router) { 
    this.truckService.getAllTruck().then((data) => {
      this.truckList = data;
      
    });
    this.selectedTruck= {
      truckID: "",
      tare: undefined,
      capacity: undefined,
      maxBatteryCapacity: undefined,
      autonomy: undefined,
      fastChargeTime: undefined
    }
  }


  ngOnInit(): void {
    
  }

  onTruckSelected($event: any){

    let test = this.truckList.find(element => element.truckID == this.selectedTruckOption);
    this.selectedTruck = test;
    
  }


  goToCreateTruck() {
    this.router.navigate(['Logistics/Truck/CreateTruck']);
    
  }
  goToEditTruck(truckID:string) {
    this.router.navigate(['Logistics/Truck/EditTruck', truckID]);
    
  } 
  deleteTruck(truckID:string) {
    this.truckService.deleteTruck(truckID);
    window.location.reload();
    
    
  }

}
