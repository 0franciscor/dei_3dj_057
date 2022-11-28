import { Component, Inject, OnInit } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { CreateTruckComponentDialog } from 'src/app/components/Logistics/truck/create-truck/create-truck.component';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';
import { DialogData } from '../edit-warehouse/edit-warehouse.component';

@Component({
  selector: 'app-create-warehouse',
  templateUrl: './create-warehouse.component.html',
  styleUrls: ['./create-warehouse.component.css']
})


export class CreateWarehouseComponent implements OnInit {

  formCreateWarehouse!: FormGroup;
  constructor(public dialog: MatDialog, private warehouseService: WarehouseService, private fb: FormBuilder, private router:Router) { }

  ngOnInit(): void {
    this.formCreateWarehouse = this.fb.group({
      Id: new FormControl('', [Validators.required, Validators.minLength(3),Validators.maxLength(3)]),
      Address:new FormControl('', [Validators.required]),
      Altitude:new FormControl('', [Validators.required, Validators.min(0),Validators.max(13000)]),
      Latitude:new FormControl('', [Validators.required,Validators.maxLength(11)]),
      Longitude:new FormControl('', [Validators.required,Validators.maxLength(12)]),
      Designation:new FormControl('', [Validators.required, Validators.maxLength(50)])

    });
  }

  async onSubmit() {
    let answer = await this.warehouseService.createWarehouse(this.formCreateWarehouse.value);
    let message = "Warehouse created successfully";

    if(answer.status != 201){
      message = "Error creating warehouse";
    }
    const dialogRef = this.dialog.open(CreateWarehouseComponentDialog, {
      width: '250px',
      data: {
        name: this.formCreateWarehouse.value.Id,
        message: message},

    });

    dialogRef.afterClosed().subscribe(result => {
      if(answer.status == 200)
        this.router.navigate(['Logistics/Home/WarehouseManager']);
      
    });
  }

  get Id() {
    return this.formCreateWarehouse.get('Id');
  }
  
  get Address() {
    return this.formCreateWarehouse.get('Address');
  }

  get Altitude() {
    return this.formCreateWarehouse.get('Altitude');
  }
  
  get Latitude() {
    return this.formCreateWarehouse.get('Latitude');
  }
  
  get Longitude() {
    return this.formCreateWarehouse.get('Longitude');
  }
  
  get Designation() {
    return this.formCreateWarehouse.get('Designation');
  }




}

@Component({
  selector: 'app-create-warehouse',
  templateUrl: 'create-warehouse.dialog.component.html',
})
export class CreateWarehouseComponentDialog {
  constructor(
    public dialogRef: MatDialogRef<CreateWarehouseComponentDialog>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData,
  ) {}

  onOk(): void {
    this.dialogRef.close();
  }
}

