import { Component, OnInit, Inject } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { DeliveryService } from 'src/app/Services/DeliveryService/delivery.service';
import { MatDialogRef, MatDialog, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { DialogData } from '../create-delivery/create-delivery.component';
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-edit-delivery',
  templateUrl: './edit-delivery.component.html',
  styleUrls: ['./edit-delivery.component.css']
})
export class EditDeliveryComponent implements OnInit {

  formEditDelivery!: FormGroup;
  minDate: Date;

  constructor(private loginService:LoginService,private dialog: MatDialog, private route: ActivatedRoute, private deliveryService: DeliveryService, private fb: FormBuilder, private router: Router) {
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

  goBack(){
    this.router.navigate(['WarehouseManagement/Home/WarehouseManager']);
  }

  isAuth: boolean = false;
  authorizedRoles: string[] = ["whMan","admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(!this.authorizedRoles.includes(role)){
      this.router.navigate(['/']);
      return false
    }
    else
      return true;
    
  }

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();
    const deliveryID = this.route.snapshot.paramMap.get('id');
    this.formEditDelivery = this.fb.group({
      deliveryID: this.selectedDelivery.deliveryID,
      deliveryDate: this.selectedDelivery.deliveryDate,
      loadTime: this.selectedDelivery.loadTime,
      unloadTime: this.selectedDelivery.unloadTime,
      destination: this.selectedDelivery.destination,
      deliveryMass: this.selectedDelivery.deliveryMass
    });

    if (deliveryID)
      this.deliveryService.getDelivery(deliveryID).then((data) => {
        this.selectedDelivery = data;
        this.formEditDelivery = this.fb.group({
          deliveryID: this.selectedDelivery.deliveryID,
          deliveryDate: this.selectedDelivery.deliveryDate,
          loadTime: this.selectedDelivery.loadTime,
          unloadTime: this.selectedDelivery.unloadTime,
          destination: this.selectedDelivery.destination,
          deliveryMass: this.selectedDelivery.deliveryMass
        });
      });
  }

  async onSubmit() {
    let answer = await this.deliveryService.updateDelivery(this.formEditDelivery.value);
    let message = "Delivery Edited successfully";

    if (answer.status == 200)
      this.deliveryService.updateDeliveryProlog(this.formEditDelivery.value);
    else
      message = "Error updating Delivery";

    const dialogRef = this.dialog.open(EditDeliveryComponentDialog, {
      width: '250px',
      data: {
        name: this.formEditDelivery.value.deliveryID,
        message: message
      },
    });

    dialogRef.afterClosed().subscribe(result => {
      if (answer.status == 200)
        this.router.navigate(['WarehouseManagement/Delivery/GetDelivery']);

    });
  }

}

@Component({
  selector: 'app-create-delivery',
  templateUrl: 'edit-delivery.dialog.component.html',
})
export class EditDeliveryComponentDialog {
  constructor(
    public dialogRef: MatDialogRef<EditDeliveryComponentDialog>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData,
  ) { }

  ngOnInit(): void {}

  onOk(): void {
    this.dialogRef.close();
  }
}