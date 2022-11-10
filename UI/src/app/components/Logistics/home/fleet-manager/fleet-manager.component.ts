import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';


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
  
  constructor(private router: Router) { 
    this.getTruck();
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
    console.log(test)
    this.selectedTruck = test;
    
  }


  async getTruck() {
    const url = 'http://localhost:3000/api/truck/all';
    let test: any[] = [];
    await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    }).then(res => res.json().then(data => {test=data;}));

    test.forEach(element => {
      this.truckList.push(element);
    });
    return test;
  }

  goToCreateTruck() {
    this.router.navigate(['Logistics/Truck/CreateTruck']);
    
  }

}
