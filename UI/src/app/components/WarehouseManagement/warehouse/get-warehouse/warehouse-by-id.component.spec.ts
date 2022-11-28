import { ComponentFixture, TestBed } from '@angular/core/testing';

import { WarehouseByIdComponent } from './warehouse-by-id.component';

describe('WarehouseByIdComponent', () => {
  let component: WarehouseByIdComponent;
  let fixture: ComponentFixture<WarehouseByIdComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ WarehouseByIdComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(WarehouseByIdComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
