import { Component, OnInit } from '@angular/core';
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-log-in',
  templateUrl: './log-in.component.html',
  styleUrls: ['./log-in.component.css']
})
export class LogInComponent implements OnInit {
  hide = true;
  constructor(private loginService:LoginService) { }

  ngOnInit(): void {
  }
  
  onSubmit() {
    console.log("Submitted");
    this.loginService.login();
  }

}
