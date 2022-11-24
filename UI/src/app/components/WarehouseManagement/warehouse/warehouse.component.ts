import { Component, OnInit } from '@angular/core';
import {WarehouseService} from "../../../Services/WarehouseService/warehouse.service";
import {FormBuilder, FormGroup} from "@angular/forms";

@Component({
  selector: 'app-warehouse',
  templateUrl: './warehouse.component.html',
  styleUrls: ['./warehouse.component.css']
})
export class WarehouseComponent implements OnInit {

  formCreateWarehouse!: FormGroup;
  constructor(private warehouseService: WarehouseService, private fb: FormBuilder) { }

  ngOnInit(): void {
    this.formCreateWarehouse = this.fb.group({
      Id: [''],
      Address:[''],
      Altitude:[''],
      Latitude:[''],
      Longitude:[''],
      Designation:['']

    });
  }

  onSubmit() {
    this.warehouseService.createWarehouse(this.formCreateWarehouse.value)
  }

}
