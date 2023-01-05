import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormGroup, FormControl, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { AdminService } from 'src/app/Services/AdminService/admin.service';
import { LoginService } from 'src/app/Services/LoginService/login.service';

import { CancelUserComponent } from './cancel-user.component';
export interface Role {
  roleId: string;
  name: string;
}
describe('CancelUserComponent', () => {
  let component: CancelUserComponent;
  let fixture: ComponentFixture<CancelUserComponent>;
  let loginService: LoginService;
  let adminService: AdminService;
  const dialogMock = {
    close: () => { }
  };
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CancelUserComponent ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule,RouterTestingModule.withRoutes([])],
        providers: [{provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    })
    .compileComponents();

    fixture = TestBed.createComponent(CancelUserComponent);
    loginService = TestBed.inject(LoginService);
    adminService = TestBed.inject(AdminService);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {

    let userList = [{
        id: "1",
        firstName: "test",
        lastName: "test",
        email: "test",
        phoneNumber: "test",
        password: "test",
        role: "test"
        },
        {
            id: "2",
            firstName: "test",
            lastName: "test",
            email: "test",
            phoneNumber: "test",
            password: "test",
            role: "test"
        },
        {
            id: "3",
            firstName: "test",
            lastName: "test",
            email: "test",
            phoneNumber: "test",
            password: "test",
            role: "admin"
        }]
    const fetchSpy = spyOn<any>(loginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    const fetchSpy2 = spyOn<any>(adminService, 'getAllUsers').and.returnValue(Promise.resolve(userList));
    component.ngOnInit();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should encrypt user info', () => {
  
    component.selectedUser= {firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"};
    component.encryptUserInfo();
    expect(component.selectedUser.firstName).toEqual("xxxxxx");
    expect(component.selectedUser.lastName).toEqual("xxxxxx");
    expect(component.selectedUser.email).toEqual("test");
    expect(component.selectedUser.password).toEqual("test");
    expect(component.selectedUser.phoneNumber).toEqual("xxxxxxxxx");
    expect(component.selectedUser.role).toEqual("deleted");

  });



  
});




