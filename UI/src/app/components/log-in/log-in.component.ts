import { Component, OnInit, NgZone, Inject  } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { CredentialResponse, PromptMomentNotification } from 'google-one-tap';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { DialogData } from '../Logistics/truck/create-truck/create-truck.component';


@Component({
  selector: 'app-log-in',
  templateUrl: './log-in.component.html',
  styleUrls: ['./log-in.component.css']
})
export class LogInComponent implements OnInit {
  hide = true;
  constructor(private loginService:LoginService, private router:Router,public dialog: MatDialog, private _ngZone: NgZone) { }
  formLogin!: FormGroup;

  isAuth: boolean = true;
  
  async isAuthenticated() {
    const role = await this.loginService.getRole();
    
    if(role=="whMan" ){
      this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']);
      return true;
    }
      
    else if(role=="logMan"){
      this.router.navigate(['/Logistics/Home/LogisticsManager']);
      return true;
    }
      
    else if(role=="fltMan"){
      this.router.navigate(['/Logistics/Home/FleetManager']);
      return true;
    }
    return false
    
  }

  clientId = "598640220043-j4v51sbat7nft28jqi165dltsq2dlrm9.apps.googleusercontent.com";

  async ngOnInit() {
    this.isAuth = false;
    
    //TODO: check if admin, if so, redirect to admin page
   
    this.formLogin = new FormGroup({
      userId: new FormControl('', [Validators.required]),
      password: new FormControl('', [Validators.required]),
    });


     // @ts-ignore
     window.onGoogleLibraryLoad = () => {
      
      // @ts-ignore
      google.accounts.id.initialize({
        client_id: this.clientId,
        callback: this.handleCredentialResponse.bind(this),
        auto_select: false,
        cancel_on_tap_outside: true
      });
      // @ts-ignore
      google.accounts.id.renderButton(
      // @ts-ignore
      document.getElementById("googleButtonDiv"),
        { theme: "outline", size: "large", width: "100%" } 
      );
      // @ts-ignore
      google.accounts.id.prompt((notification: PromptMomentNotification) => {});
    };

  }

  async handleCredentialResponse(response: CredentialResponse) {
    if (response.credential) {
      await this.loginService.loginWithGoogle(response.credential)
      window.location.reload();
      this.router.navigate(['/']);
    } 
  }
  
  async onSubmit() {
    if(this.formLogin.valid){
      await this.loginService.login(this.formLogin.value);
      window.location.reload();
      this.router.navigate(['/']);
    }      
  }

  async popUp(){
    this.dialog.open(CreateRGPDComponentDialog, {
      width: '600px',
      data: {},

    });
  }
}

@Component({
  selector: 'app-log-in',
  templateUrl: 'log-in.dialog.component.html',
})
export class CreateRGPDComponentDialog {
  constructor(
    public dialogRef: MatDialogRef<CreateRGPDComponentDialog>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData,
  ) {}

  ngOnInit(): void {}

  onOk(): void {
    this.dialogRef.close();
  }
}