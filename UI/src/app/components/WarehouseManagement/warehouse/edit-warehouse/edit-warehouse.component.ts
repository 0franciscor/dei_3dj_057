import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ActivatedRoute, Router } from '@angular/router';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';

export interface DialogData {
  name: string;
  message: string;
}

interface warehouse{
  warehouseId: string;
  address: string;
  altitude: number;
  latitude: string;
  longitude: string;
  description: string;
  cityId: string;
}

@Component({
  selector: 'app-edit-warehouse',
  templateUrl: './edit-warehouse.component.html',
  styleUrls: ['./edit-warehouse.component.css']
})
export class EditWarehouseComponent implements OnInit {

  formEditWarehouse!: FormGroup;


  constructor(public dialog: MatDialog,private route: ActivatedRoute, private warehouseService: WarehouseService, private fb: FormBuilder, private router: Router) { 
    
  }

  selectedWarehouse = {
    WarehouseId: "",
    Address: undefined,
    Altitude: undefined,
    Latitude: undefined,
    Longitude: undefined,
    Designation: undefined,
  }

  ngOnInit(): void {
    const warehouseID = this.route.snapshot.paramMap.get('id');
    this.formEditWarehouse = this.fb.group({
      WarehouseId: this.selectedWarehouse.WarehouseId,
      Altitude: this.selectedWarehouse.Altitude,
      Address: this.selectedWarehouse.Address,
      Latitude: this.selectedWarehouse.Latitude,
      Longitude: this.selectedWarehouse.Longitude,
      Designation: this.selectedWarehouse.Designation
    });

    if (warehouseID)
      this.warehouseService.getWarehouse(warehouseID).then((data) => {
        this.selectedWarehouse = data;
        this.formEditWarehouse = this.fb.group({
          WarehouseId: this.selectedWarehouse.WarehouseId,
          Altitude: this.selectedWarehouse.Altitude,
          Address: this.selectedWarehouse.Address,
          Latitude: this.selectedWarehouse.Latitude,
          Longitude: this.selectedWarehouse.Longitude,
          Designation: this.selectedWarehouse.Designation
        });
      });
  }

  async onSubmit() {
    let answer = await this.warehouseService.updateWarehouse(this.formEditWarehouse.value);
    let message = "Warehouse updated successfully";

    if(answer.status != 200){
      message = "Error updating warehouse";
    }
    const dialogRef = this.dialog.open(EditWarehouseComponentDialog, {
      width: '250px',
      data: {
        name: this.selectedWarehouse.WarehouseId,
        message: message},
    });

    dialogRef.afterClosed().subscribe(result => {
      if(answer.status == 200)
        this.router.navigate(['WarehouseManagement/Home/WarehouseManager']);
      
    });
  }

}

@Component({
  selector: 'app-edit-warehouse',
  templateUrl: 'edit-warehouse.dialog.component.html',
})
  export class EditWarehouseComponentDialog {
  constructor(
    public dialogRef: MatDialogRef<EditWarehouseComponentDialog>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData,
  ) {}

  onOk(): void {
    this.dialogRef.close();
  }
}

