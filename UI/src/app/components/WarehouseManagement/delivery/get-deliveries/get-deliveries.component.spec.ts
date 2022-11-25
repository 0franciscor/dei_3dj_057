import { ComponentFixture, TestBed } from '@angular/core/testing';

import { GetDeliveriesComponent } from './get-deliveries.component';

describe('GetDeliveriesComponent', () => {
  let component: GetDeliveriesComponent;
  let fixture: ComponentFixture<GetDeliveriesComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GetDeliveriesComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(GetDeliveriesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
