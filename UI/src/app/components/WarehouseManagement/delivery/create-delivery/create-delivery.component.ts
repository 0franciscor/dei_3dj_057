import { Component, OnInit } from '@angular/core';
import { DeliveryService } from "src/app/Services/DeliveryService/delivery.service";
import { FormBuilder, FormGroup, FormControl, Validators } from "@angular/forms";

@Component({
  selector: 'app-create-delivery',
  templateUrl: './create-delivery.component.html',
  styleUrls: ['./create-delivery.component.css'],
})

export class CreateDeliveryComponent implements OnInit {

  formCreateDelivery!: FormGroup;
  minDate: Date;

  constructor(private deliveryService: DeliveryService, private fb: FormBuilder) {
    const currentYear = new Date().getFullYear();
    const currentMonth = new Date().getMonth();
    const currentDay = new Date().getDate();
    this.minDate = new Date(currentYear, currentMonth, currentDay);
  }

  ngOnInit(): void {
    this.formCreateDelivery = this.fb.group({
      deliveryID: new FormControl('', [Validators.required]),
      deliveryDate: new FormControl('', [Validators.required]),
      loadTime: new FormControl('', [Validators.required, Validators.min(0)]),
      unloadTime: new FormControl('', [Validators.required, Validators.min(0)]),
      destination: new FormControl('', [Validators.required]),
      deliveryMass: new FormControl('', [Validators.required, Validators.min(0)]),
    });
  }

  async onSubmit() {
    console.log(this.formCreateDelivery.value);
    let answer = await this.deliveryService.createDelivery(this.formCreateDelivery.value);
    if(answer.status == 200){
      this.deliveryService.createDeliveryProlog(answer.json());
    }
  }

  get deliveryID() {
    return this.formCreateDelivery.get('deliveryID');
  }

  get deliveryDate() {
    return this.formCreateDelivery.get('deliveryDate');
  }

  get loadTime() {
    console.log(this.formCreateDelivery.get('loadTime'));
    return this.formCreateDelivery.get('loadTime');
  }

  get unloadTime() {
    return this.formCreateDelivery.get('unloadTime');
  }

  get destination() {
    return this.formCreateDelivery.get('destination');
  }

  get deliveryMass() {
    return this.formCreateDelivery.get('deliveryMass');
  }
}