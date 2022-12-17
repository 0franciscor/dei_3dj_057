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

import { ToolBarComponent } from './tool-bar.component';

describe('ToolBarComponent', () => {
  let component: ToolBarComponent;
  let fixture: ComponentFixture<ToolBarComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ToolBarComponent ],
      imports:[MatToolbarModule,MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule,MatIconModule,RouterTestingModule.withRoutes(
        [
          {path: 'Logistics/Home/FleetManager', redirectTo: ''},
          {path: 'Logistics/Home/LogisticsManager', redirectTo: ''},
          {path: 'WarehouseManagement/Home/WarehouseManager', redirectTo: ''},
          {path: 'home', redirectTo: ''},
      
        ])]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ToolBarComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should go to home', () => {
    component.goHome();
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
});
