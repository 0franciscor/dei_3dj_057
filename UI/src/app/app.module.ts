import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { LogInComponent } from './components/log-in/log-in.component';
import { RegisterComponent } from './components/register/register.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { FlexLayoutModule } from '@angular/flex-layout';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatCardModule } from '@angular/material/card';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { MatDialogModule } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { ToolBarComponent } from './components/tool-bar/tool-bar.component';
import { CreateTruckComponent } from './components/Logistics/truck/create-truck/create-truck.component';
import { FleetManagerComponent, DeleteTruckComponentDialog } from './components/Logistics/home/fleet-manager/fleet-manager.component';
import { LogisticsManagerComponent } from './components/Logistics/home/logistics-manager/logistics-manager.component';
import { WarehouseManagerComponent } from './components/WarehouseManagement/home/warehouse-manager/warehouse-manager.component';
import { RoadNetworkComponent } from './components/Logistics/road-network/road-network.component';
import { CreatePathComponent } from './components/Logistics/path/create-path/create-path.component';
import { WarehouseComponent } from './components/WarehouseManagement/warehouse/warehouse.component';
import { CreateDeliveryComponent } from './components/WarehouseManagement/delivery/create-delivery/create-delivery.component';
import { WarehouseByIdComponent } from './components/WarehouseManagement/warehouse/getById/warehouse-by-id/warehouse-by-id.component';
import { GetDeliveriesComponent } from './components/WarehouseManagement/delivery/get-deliveries/get-deliveries.component';
import { EditTruckComponent, EditTruckComponentDialog } from './components/Logistics/truck/edit-truck/edit-truck.component';

@NgModule({
  declarations: [
    AppComponent,
    LogInComponent,
    RegisterComponent,
    ToolBarComponent,
    CreateTruckComponent,
    FleetManagerComponent,
    LogisticsManagerComponent,
    WarehouseManagerComponent,
    RoadNetworkComponent,
    CreatePathComponent,
    WarehouseComponent,
    CreateDeliveryComponent,
    WarehouseByIdComponent,
    GetDeliveriesComponent,
    EditTruckComponent,
    EditTruckComponentDialog,
    DeleteTruckComponentDialog
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    BrowserAnimationsModule,
    FlexLayoutModule,
    FormsModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatCardModule,
    MatToolbarModule,
    MatInputModule,
    MatIconModule,
    MatDatepickerModule,
    MatNativeDateModule,
    MatDialogModule,
    MatButtonModule
  ],
  providers: [],
  bootstrap: [AppComponent],
})
export class AppModule { }
