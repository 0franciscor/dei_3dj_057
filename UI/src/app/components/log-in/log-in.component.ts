import { Component, NgZone, OnInit } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { CredentialResponse, PromptMomentNotification } from 'google-one-tap';
import { LoginService } from 'src/app/Services/LoginService/login.service';


@Component({
  selector: 'app-log-in',
  templateUrl: './log-in.component.html',
  styleUrls: ['./log-in.component.css']
})
export class LogInComponent implements OnInit {
  hide = true;
  
  constructor(private ngZone:NgZone,private loginService:LoginService, private router:Router,public dialog: MatDialog, private _ngZone: NgZone) { }
  formLogin!: FormGroup;
  isDisabled = true;
  isAuth: boolean = true;
  
  async isAuthenticated() {
    const role = await this.loginService.getRole();

    if(role=="admin"){
      this.ngZone.run(() => this.router.navigate(['/Admin/Home']));
      return true;
    }
    
    else if(role=="whMan" ){
      this.ngZone.run(() => this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']));
      return true;
    }
      
    else if(role=="logMan"){
      this.ngZone.run(() => this.router.navigate(['/Logistics/Home/LogisticsManager']));
      return true;
    }
      
    else if(role=="fltMan"){
      this.ngZone.run(() => this.router.navigate(['/Logistics/Home/FleetManager']));
      return true;
    }
    return false
    
  }

  clientId = "598640220043-j4v51sbat7nft28jqi165dltsq2dlrm9.apps.googleusercontent.com";

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();
   
    if(!this.isAuth){
      this.formLogin = new FormGroup({
        email: new FormControl('', [Validators.required]),
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
    

  }

  async handleCredentialResponse(response: CredentialResponse) {
    if (response.credential) {
      await this.loginService.loginWithGoogle(response.credential)
      window.location.reload();
      this.ngZone.run(() => this.router.navigate(['/']));
    } 
  }
  
  async onSubmit() {
    if(this.formLogin.valid){
      await this.loginService.login(this.formLogin.value);
      window.location.reload();
      this.ngZone.run(() => this.router.navigate(['/']));
    }      
  }

 async isAvaiable(event : any){
  
    if (event.target.checked ) {
      this.isDisabled = false;
    }
    else{
      this.isDisabled = true;
    }
 }



  
}

