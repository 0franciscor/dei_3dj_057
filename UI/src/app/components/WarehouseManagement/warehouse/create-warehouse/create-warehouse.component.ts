import { Component, OnInit } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { FormBuilder, FormGroup } from '@angular/forms';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';

@Component({
  selector: 'app-create-warehouse',
  templateUrl: './create-warehouse.component.html',
  styleUrls: ['./create-warehouse.component.css']
})


export class CreateWarehouseComponent implements OnInit {

  formCreateWarehouse!: FormGroup;
  constructor(private warehouseService: WarehouseService, private fb: FormBuilder) { }

  ngOnInit(): void {
    this.formCreateWarehouse = this.fb.group({
      Id: [new FormControl('', [Validators.required, Validators.minLength(3),Validators.maxLength(3)])],
      Address:[new FormControl('', [Validators.required])],
      Altitude:[new FormControl('', [Validators.required, Validators.min(0),Validators.max(13000)])],
      Latitude:[new FormControl('', [Validators.required,Validators.maxLength(11)])],
      Longitude:[new FormControl('', [Validators.required,Validators.maxLength(12)])],
      Designation:[new FormControl('', [Validators.required, Validators.maxLength(50)])]

    });
  }

  onSubmit() {
    this.warehouseService.createWarehouse(this.formCreateWarehouse.value)
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
