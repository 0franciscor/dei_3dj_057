import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
// const http = require('https');
import fetch from 'node-fetch';
@Component({
  selector: 'app-create-truck',
  templateUrl: './create-truck.component.html',
  styleUrls: ['./create-truck.component.css']
})
export class CreateTruckComponent implements OnInit {

  formCreateTruck!: FormGroup;
  constructor(private fb: FormBuilder) {}

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
    console.log(this.formCreateTruck.value);

    //send formCreateTruck.value to API
    const url = 'https://localhost:3000/api/truck/';
    const data = this.formCreateTruck.value;
    // const httpAgent = new http.Agent({ rejectUnauthorized: false });
    fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
      // agent: httpAgent
    })
  }

}
