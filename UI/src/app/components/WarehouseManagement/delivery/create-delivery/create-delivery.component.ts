import { Component, OnInit } from '@angular/core';
import { DeliveryService } from "src/app/Services/DeliveryService/delivery.service";
import { FormBuilder, FormGroup } from "@angular/forms";

@Component({
  selector: 'app-create-delivery',
  templateUrl: './create-delivery.component.html',
  styleUrls: ['./create-delivery.component.css']
})
export class CreateDeliveryComponent implements OnInit {

  formCreateDelivery!: FormGroup;
  constructor(private deliveryService: DeliveryService, private fb: FormBuilder) { }

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
    this.deliveryService.createDelivery(this.formCreateDelivery.value);
  }
}