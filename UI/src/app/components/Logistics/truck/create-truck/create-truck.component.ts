import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { TruckService } from 'src/app/Services/TruckService/truck.service';


@Component({
  selector: 'app-create-truck',
  templateUrl: './create-truck.component.html',
  styleUrls: ['./create-truck.component.css']
})
export class CreateTruckComponent implements OnInit {

  formCreateTruck!: FormGroup;
  constructor(private truckService: TruckService,private fb: FormBuilder) {}

  ngOnInit() {
    this.formCreateTruck = this.fb.group({
      truckID: [''],
      tare: [''],
      capacity: [''],
      maxBatteryCapacity: [''],
      autonomy: [''],
      fastChargeTime: ['']
    });

  }

  onSubmit() {
    this.truckService.createTruck(this.formCreateTruck.value);
  
   
  }

}
