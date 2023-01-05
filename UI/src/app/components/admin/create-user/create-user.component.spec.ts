import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormGroup, FormControl } from '@angular/forms';
import { AdminService } from 'src/app/Services/AdminService/admin.service';
import { LoginService } from 'src/app/Services/LoginService/login.service';

import { CreateUserComponent } from './create-user.component';
export interface Role {
  roleId: string;
  name: string;
}
describe('CreateUserComponent', () => {
  let component: CreateUserComponent;
  let fixture: ComponentFixture<CreateUserComponent>;
  let loginService: LoginService;
  let adminService: AdminService;
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CreateUserComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(CreateUserComponent);
    loginService = TestBed.inject(LoginService);
    adminService = TestBed.inject(AdminService);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be authenticated with admin role', async () => {

    const fetchSpy = spyOn<any>(loginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    const response = await component.isAuthenticated();
    expect(response).toBeTrue();
    expect(fetchSpy).toHaveBeenCalled();

  });

  it('should create', () => {
    const fetchSpy = spyOn<any>(loginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    component.ngOnInit();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should submit', () => {
    
    let roles :Role[] = [
      {
        roleId: "1",
        name: "admin"
      },
      {
        roleId: "2",
        name: "user"
      }
    ];


    component.roleList = roles;
    component.formCreateUser = new FormGroup({
      firstName: new FormControl(''),
      lastName: new FormControl(''),
      email: new FormControl(''),
      password: new FormControl(''),
      role: new FormControl(''),
      phoneNumber: new FormControl('')
    });
    component.formCreateUser.controls['firstName'].setValue('test');
    component.formCreateUser.controls['lastName'].setValue('test');
    component.formCreateUser.controls['email'].setValue('test');
    component.formCreateUser.controls['password'].setValue('test');
    component.formCreateUser.controls['role'].setValue('user');
    component.formCreateUser.controls['phoneNumber'].setValue('test');
    const fetchSpy = spyOn<any>(adminService, 'createUser').and.returnValue(Promise.resolve({status:201}));
    component.onSubmit();

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
