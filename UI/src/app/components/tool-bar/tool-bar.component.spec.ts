import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatToolbarModule } from '@angular/material/toolbar';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { LoginService } from 'src/app/Services/LoginService/login.service';

import { ToolBarComponent } from './tool-bar.component';

describe('ToolBarComponent', () => {
  let component: ToolBarComponent;
  let fixture: ComponentFixture<ToolBarComponent>;
  let service : LoginService;
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ToolBarComponent ],
      imports:[MatToolbarModule,MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule,MatIconModule,RouterTestingModule.withRoutes(
        [
          {path: 'Logistics/Home/FleetManager', redirectTo: ''},
          {path: 'Logistics/Home/LogisticsManager', redirectTo: ''},
          {path: 'WarehouseManagement/Home/WarehouseManager', redirectTo: ''},
          {path: 'home', redirectTo: ''},
          {path: 'TermsAndConditions', redirectTo: ''},
          {path: 'UserManagement/CancelAccount', redirectTo: ''},
          {path: 'Admin/Home', redirectTo: ''},
      
        ])]
    })
    .compileComponents();

    service = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(ToolBarComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('cookie with jwt', () => {
    spyOnProperty(document, 'cookie').and.returnValue('jwt=123;abc=456');
    component.checkCookie();
  });

  it('cookie without jwt', () => {
    spyOnProperty(document, 'cookie').and.returnValue('jwt=;abc=456');
    component.checkCookie();
  });


  it('should logout', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    component.logout();
    expect(component).toBeTruthy();

  });

  it('should be admin', () => {
    const fetchSpy = spyOn<any>(service, 'getRole').and.returnValue(Promise.resolve("admin"));
    component.ngOnInit();
    expect(component).toBeTruthy();
  });


  it('should go to home as logged out', () => {
    const fetchSpy = spyOn<any>(service, 'getRole').and.returnValue(Promise.resolve(""));
    component.goHome();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should go to home as fltMan', () => {
    const fetchSpy = spyOn<any>(service, 'getRole').and.returnValue(Promise.resolve("fltMan"));
    
    component.goHome();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should go to home as logMan', () => {
    const fetchSpy = spyOn<any>(service, 'getRole').and.returnValue(Promise.resolve("logMan"));
    
    component.goHome();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should go to home as whMan', () => {
    const fetchSpy = spyOn<any>(service, 'getRole').and.returnValue(Promise.resolve("whMan"));
    
    component.goHome();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should go to home as admin', () => {
    const fetchSpy = spyOn<any>(service, 'getRole').and.returnValue(Promise.resolve("admin"));
    
    component.goHome();
    expect(fetchSpy).toHaveBeenCalled();
    expect(component).toBeTruthy();
  });

  it('should go to FM home', () => {
    component.goToFMHome();
    expect(component).toBeTruthy();
  });

  it('should go to LM home', () => {
    component.goToLMHome();
    expect(component).toBeTruthy();
  });

  it('should go to WM home', () => {
    component.goToWMHome();
    expect(component).toBeTruthy();
  });

  it('should go to terms and conditions', () => {
    component.goToTermsAndConditions();
    expect(component).toBeTruthy();
  });

  it('should go to user management', () => {
    component.goToUserManagement();
    expect(component).toBeTruthy();
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

