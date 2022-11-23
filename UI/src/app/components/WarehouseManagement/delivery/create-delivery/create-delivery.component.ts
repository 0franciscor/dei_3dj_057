import { Component, OnInit } from '@angular/core';
import { DeliveryService } from "src/app/Services/DeliveryService/delivery.service";
import { FormBuilder, FormGroup } from "@angular/forms";

interface Delivery{
  deliveryID: string;
  deliveryDate: Date;
  loadTime: number;
  unloadTime: number;
  destination: string;
  deliveryMass: number;
}

@Component({
  selector: 'app-create-delivery',
  templateUrl: './create-delivery.component.html',
  styleUrls: ['./create-delivery.component.css']
})
export class CreateDeliveryComponent implements OnInit {

  formCreateDelivery!: FormGroup;
  minDate: Date;
  
  constructor(private deliveryService: DeliveryService, private fb: FormBuilder) {
      const currentYear = new Date().getFullYear();
      //set minimum date for today
      this.minDate = new Date(currentYear, 0, 1);
   }

  ngOnInit(): void {
    this.formCreateDelivery = this.fb.group({
      deliveryID: [''],
      deliveryDate: [''],
      loadTime: [''],
      unloadTime: [''],
      destination: [''],
      deliveryMass: [''],
    });
  }

  onSubmit() {
    console.log(this.formCreateDelivery.value);
    this.deliveryService.createDelivery(this.formCreateDelivery.value);
  }
}