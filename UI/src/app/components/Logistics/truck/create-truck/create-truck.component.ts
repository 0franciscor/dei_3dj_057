import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { TruckService } from 'src/app/Services/TruckService/truck.service';


export interface DialogData {
  name: string;
  message: string;
}

@Component({
  selector: 'app-create-truck',
  templateUrl: './create-truck.component.html',
  styleUrls: ['./create-truck.component.css'],
  providers: [TruckService]
})
export class CreateTruckComponent implements OnInit {

  formCreateTruck!: FormGroup;
  constructor(public dialog: MatDialog,private truckService: TruckService,private fb: FormBuilder,private router: Router) {}

  ngOnInit() {
    this.formCreateTruck = new FormGroup({
      truckID: new FormControl('', [Validators.required]),
      tare: new FormControl('', [Validators.required]),
      capacity: new FormControl('', [Validators.required]),
      maxBatteryCapacity: new FormControl('', [Validators.required]),
      autonomy: new FormControl('', [Validators.required]),
      fastChargeTime: new FormControl('', [Validators.required])
    });


  }

  async onSubmit() {
    if(this.formCreateTruck.valid){
      let answer = await this.truckService.createTruck(this.formCreateTruck.value);
      let message = "Truck created successfully";
      if(answer.status != 201){
        message = "Error creating truck";
      }
      const dialogRef = this.dialog.open(CreateTruckComponentDialog, {
        width: '250px',
        data: {
          name: this.formCreateTruck.value.truckID,
          message: message},
      });
  
      dialogRef.afterClosed().subscribe(result => {
        if(answer.status == 200)
          this.router.navigate(['Logistics/Home/FleetManager']);
        
      });
     
    }
    
  }

}


@Component({
  selector: 'app-create-truck',
  templateUrl: 'create-truck.dialog.component.html',
})
export class CreateTruckComponentDialog {
  constructor(
    public dialogRef: MatDialogRef<CreateTruckComponentDialog>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData,
  ) {}
  
  ngOnInit(): void {}

  onOk(): void {
    this.dialogRef.close();
  }
}
