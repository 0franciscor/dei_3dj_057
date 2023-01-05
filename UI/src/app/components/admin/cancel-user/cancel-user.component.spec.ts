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

  it('should select user', () => {
      component.accountList = [{firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"}];
      component.selectedUserOption = "test";
      component.onUserSelected();
      expect(component).toBeTruthy();
  


  });

  it('should go back', () => {
    component.goBack();
    expect(component).toBeTruthy();

  });

  it('should submit user', async () => {
    const fetchSpy2 = spyOn<any>(adminService, 'updateUser').and.returnValue(Promise.resolve({status:200}));
    component.selectedUser= {firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"};
    await component.onSubmit();
    expect(fetchSpy2).toHaveBeenCalled();
  });



  
});

describe('AdminService', () => {
  let service: AdminService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(AdminService);
  });

  
  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('cookie with jwt', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const cookie = service.getJwt();
    expect(cookie).toEqual('jwt=123');
    
  });

  it('cookie without jwt', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('abc=123');
    const cookie = service.getJwt();
    expect(cookie).toEqual('jwt=');

  });

  it('should create a user', async () => {
    const response = {
      "status": 201,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.createUser("data");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.createUser("data");
  });

  it('should create a user error', async () => {
    const response = {
      "status": 401,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.createUser("data");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.createUser("data");
  });

  it('should get all user', async () => {

    const response = {
      "status": 200,
      json() {
        return [{firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"}];
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getAllUsers();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getAllUsers();


  });

  it('should not get all user', async () => {

    const response = {
      "status": 401,
      json() {
        return [{firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"}];
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getAllUsers();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getAllUsers();


  });

  it('should get a user', async () => {

    const response = {
      "status": 200,
      json() {
        return {firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getUser();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getUser();


  });

  it('should not get a user', async () => {

    const response = {
      "status": 401,
      json() {
        return {firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getUser();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getUser();


  });

  it('should update a user', async () => {
  
    const response = {
      "status": 200,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.updateUser("data");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.updateUser("data");

  });

  it('should get all role', async () => {

    const response = {
      "status": 200,
      json() {
        return [{role:"test"}];
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getAllRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getAllRole();


  });

  it('should not get all role', async () => {

    const response = {
      "status": 401,
      json() {
        return [{role:"test"}];
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getAllRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getAllRole();


  });



  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null, "cookie");
    expect(status.status).toEqual(404);

  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null", "cookie");
    expect(status.status).toEqual(404);
  });
});

describe('LoginService', () => {
  let service: LoginService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(LoginService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('get role with jwt cookie', async () => {
    const response = {
      "status": 200,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const role = await service.getRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  
  it('get role with null jwt cookie', async () => {
    const response = {
      "status": 401,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('');
    const role = await service.getRole();
    expect(fetchSpy).not.toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  it('get invalid role with jwt cookie', async () => {
    const response = {
      "status": 401,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const role = await service.getRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  it('should login', async () => {

    const response = {
      "status": 200,
      json() {
        return {token: "test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const login = await service.login("test");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.login("test");

  });

  it('should login with google', async () => {

    const response = {
      "status": 200,
      json() {
        return {token: "test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const login = await service.loginWithGoogle("test");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.loginWithGoogle("test");

  });




  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null, "cookie");
    expect(status.status).toEqual(404);

  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null", "cookie");
    expect(status.status).toEqual(404);
  });
});




