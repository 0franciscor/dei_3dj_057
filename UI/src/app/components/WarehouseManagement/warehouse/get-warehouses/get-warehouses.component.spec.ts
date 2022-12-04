import { ComponentFixture, TestBed } from '@angular/core/testing';

import { GetWarehousesComponent } from './get-warehouses.component';

describe('GetWarehousesComponent', () => {
  let component: GetWarehousesComponent;
  let fixture: ComponentFixture<GetWarehousesComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GetWarehousesComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(GetWarehousesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
