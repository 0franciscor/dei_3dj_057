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

  public selectedObject : Truck | undefined;

  public truckList: Truck[] = [];
  
  constructor(private router: Router) { 
    this.getTruck().then(data => {
      data.forEach(element => {
        this.truckList.push(element);
      });
    });

    console.log(this.truckList.length);

  }

  test() {
    console.log(this.selectedObject?.truckID);
  }

  ngOnInit(): void {
    
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
    
    return test;
  }

  goToCreateTruck() {
    this.router.navigate(['Logistics/Truck/CreateTruck']);
    
  }

}
