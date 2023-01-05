import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { AdminService } from 'src/app/Services/AdminService/admin.service';
import { LoginService } from 'src/app/Services/LoginService/login.service';

import { CancelAccountComponent } from './cancel-account.component';

describe('CancelAccountComponent', () => {
  let component: CancelAccountComponent;
  let fixture: ComponentFixture<CancelAccountComponent>;
  let fakeLoginService: LoginService;
  let fakeAdminService: AdminService;
  const dialogMock = {
    close: () => { }
  };
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CancelAccountComponent ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule,RouterTestingModule.withRoutes([])],
      providers: [{provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    })
    .compileComponents();

    fixture = TestBed.createComponent(CancelAccountComponent);
    fakeLoginService = TestBed.inject(LoginService);
    fakeAdminService = TestBed.inject(AdminService);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be authenticated with fltMan role', async () => {

    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("fltMan"));
    const response = await component.isAuthenticated();
    expect(response).toBeTrue();
    expect(fetchSpy).toHaveBeenCalled();

  });

  it('should create', () => {
    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("fltMan"));
    component.ngOnInit();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should encrypt user info', () => {
  
    component.myUser = {firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"};
    component.encryptUserInfo();
    expect(component.myUser.firstName).toEqual("xxxxxx");
    expect(component.myUser.lastName).toEqual("xxxxxx");
    expect(component.myUser.email).toEqual("test");
    expect(component.myUser.password).toEqual("test");
    expect(component.myUser.phoneNumber).toEqual("xxxxxxxxx");
    expect(component.myUser.role).toEqual("deleted");

  });

  it('should submit', () => {
    component.myUser = {firstName:"test", lastName:"test", email:"test", password:"test", phoneNumber:"test", role:"test"};
    const fetchSpy = spyOn<any>(fakeAdminService, 'updateUser').and.returnValue(Promise.resolve({status:200}));
    component.onSubmit();
    expect(fetchSpy).toHaveBeenCalled();
  });

  it('should go back', () => {

    component.goBack();
    expect(component).toBeTruthy();

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

