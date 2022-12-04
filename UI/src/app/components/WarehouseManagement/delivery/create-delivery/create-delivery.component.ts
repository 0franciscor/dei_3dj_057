import { Component, Inject, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { DeliveryService } from "src/app/Services/DeliveryService/delivery.service";
import { FormBuilder, FormGroup, FormControl, Validators } from "@angular/forms";
import { MatDialogRef, MatDialog, MAT_DIALOG_DATA } from '@angular/material/dialog';

export interface DialogData {
  name: string;
  message: string;
}
@Component({
  selector: 'app-create-delivery',
  templateUrl: './create-delivery.component.html',
  styleUrls: ['./create-delivery.component.css'],
})

export class CreateDeliveryComponent implements OnInit {

  formCreateDelivery!: FormGroup;
  minDate: Date;

  constructor(private dialog: MatDialog, private deliveryService: DeliveryService, private fb: FormBuilder, private router: Router) {
    const currentYear = new Date().getFullYear();
    const currentMonth = new Date().getMonth();
    const currentDay = new Date().getDate();
    this.minDate = new Date(currentYear, currentMonth, currentDay);
  }

  selectedDelivery = {
    deliveryID: "",
    deliveryDate: undefined,
    deliveryMass: undefined,
    destination: undefined,
    loadTime: undefined,
    unloadTime: undefined,
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
    let answer = await this.deliveryService.createDelivery(this.formCreateDelivery.value);
    let message = "Delivery created successfully";
    if (answer.status == 201)
      this.deliveryService.createDeliveryProlog(this.formCreateDelivery.value);
    else
      message = "Delivery creation failed";

    const dialogRef = this.dialog.open(CreateDeliveryComponentDialog, {
      width: '250px',
      data: {
        name: this.formCreateDelivery.value.deliveryID,
        message: message
      },
    });

    dialogRef.afterClosed().subscribe(result => {
      if (answer.status == 201)
        this.router.navigate(['WarehouseManagement/Home/WarehouseManager']);

    });
  }

  get deliveryID() {
    return this.formCreateDelivery.get('deliveryID');
  }

  get deliveryDate() {
    return this.formCreateDelivery.get('deliveryDate');
  }

  get loadTime() {
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

@Component({
  selector: 'app-create-delivery',
  templateUrl: 'create-delivery.dialog.component.html',
})
export class CreateDeliveryComponentDialog {
  constructor(
    public dialogRef: MatDialogRef<CreateDeliveryComponentDialog>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData,
  ) { }

  ngOnInit(): void {}

  onOk(): void {
    this.dialogRef.close();
  }
}