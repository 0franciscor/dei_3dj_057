using EletricGo.Domain.Cities;
using EletricGo.Domain.Cities.ValueObjects;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses;
using EletricGo.Domain.Warehouses.DTO;
using EletricGo.Domain.Warehouses.ValueObjects;
using EletricGo.Services;
using EletricGo.Services.Interfaces;
using Moq;

namespace WarehouseManagementTest.Unit.Services
{
    [TestFixture]
    internal class WarehouseServiceTest
    {
        private readonly string? id = "WH";

        private readonly string address = "Rua António Bernardino,47,4535-334,Porto";

        private readonly int altitude = 200;

        private readonly string latitude = "45.8746º N";

        private readonly string longitude = "79.2651º W";

        private readonly string description = "Porto";
        
        private readonly string CityId = "1";

        [Test]
        public void DefineDriverServiceConstructor()
        {
            var mockDeliveryRepo = new Moq.Mock<IWarehouseRepository>();
            var mockUnitWork = new Moq.Mock<IUnitOfWork>();

            var service = new WarehouseService(mockUnitWork.Object, mockDeliveryRepo.Object);

            Assert.That(service, Is.Not.Null);
        }


        [Test]
        public async Task GetWarehousesTest()
        {
            var mockRepository = new Moq.Mock<IWarehouseRepository>();
            mockRepository.Setup(repo => repo.GetAll()).ReturnsAsync(CreatedWarehouses());

            var mockUnit = new Moq.Mock<IUnitOfWork>();

            var service = new WarehouseService(mockUnit.Object, mockRepository.Object);

            var expectedList = await service.GetWarehouses();

            var resultList = CreatedWarehouses();

            Assert.That(resultList, Has.Count.EqualTo(expectedList.Count));
        }

        private List<Warehouse> CreatedWarehouses()
        {
            var warehouseList = new List<Warehouse>();
            warehouseList.Add(new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description), new CityId(CityId)));

            return warehouseList;
        }

        [Test]
        public async Task GetWarehouseTest()
        {
            var warehouseExpected = new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description), new CityId(CityId));

            var mockRepository = new Moq.Mock<IWarehouseRepository>();
            mockRepository.Setup(repo => repo.GetByID(new WarehouseId("WH1"))).ReturnsAsync(warehouseExpected);

            var mockUnit = new Moq.Mock<IUnitOfWork>();

            var service = new WarehouseService(mockUnit.Object, mockRepository.Object);

            var warehouseResult = await service.GetWarehouse(new WarehouseId(id + "1"));

            Assert.Multiple(() =>
            {
                Assert.That(warehouseResult.Id, Is.EqualTo(warehouseExpected.Id));
                Assert.That(warehouseResult.Address, Is.EqualTo(warehouseExpected.Address.fullAddress));
                Assert.That(warehouseResult.Designation, Is.EqualTo(warehouseExpected.Designation.designation));
                Assert.That(warehouseResult.Altitude, Is.EqualTo(warehouseExpected.Altitude.altitude));
                Assert.That(warehouseResult.Longitude, Is.EqualTo(warehouseExpected.Coordinates.longitude));
                Assert.That(warehouseResult.Latitude, Is.EqualTo(warehouseExpected.Coordinates.latitude));

            });
        }

        [Test]
        public async Task CreateWarehouseTest()
        {
            var warehouseExpected = new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description), new CityId(CityId));

            var warehouseDto = new WarehouseDto()
            {
                Id = "WH1",
                Address = address,
                Altitude = altitude,
                Latitude = latitude,
                Longitude = longitude,
                Designation = description
            };

            var mockRepository = new Moq.Mock<IWarehouseRepository>();
            var mockUnit = new Moq.Mock<IUnitOfWork>();
            mockRepository.Setup(repo => repo.Add(warehouseExpected));
            mockUnit.Setup(repo => repo.CommitAsync());

            mockRepository.Setup(repo => repo.GetByID(new WarehouseId(id + "1"))).ReturnsAsync(null as Warehouse);

            var service = new WarehouseService(mockUnit.Object, mockRepository.Object, new CityService(mockUnit.Object,new Mock<ICityRepository>().Object));

            var warehouseResult = await service.CreateWarehouse(warehouseDto);

            Assert.Multiple(() =>
            {
                Assert.That(warehouseResult.Id, Is.EqualTo(warehouseExpected.Id));
                Assert.That(warehouseResult.Address, Is.EqualTo(warehouseExpected.Address.fullAddress));
                Assert.That(warehouseResult.Altitude, Is.EqualTo(warehouseExpected.Altitude.altitude));
                Assert.That(warehouseResult.Latitude, Is.EqualTo(warehouseExpected.Coordinates.latitude));
                Assert.That(warehouseResult.Longitude, Is.EqualTo(warehouseExpected.Coordinates.longitude));
                Assert.That(warehouseResult.Designation, Is.EqualTo(warehouseExpected.Designation.designation));

            });
        }

        [Test]
        public async Task UpdateWarehouseTest()
        {
            var warehouseId = new WarehouseId(id + "1");

            var warehouseDto = new WarehouseDto()
            {
                Id = id + "1",
                Address = address,
                Altitude = 201,
                Latitude = latitude,
                Longitude = longitude,
                Designation = description
            };

            var warehouseExpected = new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(201), new Coordinates(latitude, longitude), new Designation(description), new CityId(CityId));

            var mockRepository = new Moq.Mock<IWarehouseRepository>();
            mockRepository.Setup(repo => repo.GetByID(warehouseId)).ReturnsAsync(warehouseExpected);

            var mockUnit = new Moq.Mock<IUnitOfWork>();
            
            var service = new WarehouseService(mockUnit.Object, mockRepository.Object);

            var getDel = await service.UpdateWarehouse(warehouseDto);

            Assert.That(warehouseExpected.Altitude.altitude, Is.EqualTo(warehouseDto.Altitude));
        }


        [Test]
        public async Task DeleteAsyncTest()
        {
            var warehouseId = new WarehouseId(id + "1");

            var warehouseExpected = new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(201), new Coordinates(latitude, longitude), new Designation(description), new CityId(CityId));

            var mockRepository = new Moq.Mock<IWarehouseRepository>();
            mockRepository.Setup(repo => repo.GetByID(warehouseId)).ReturnsAsync(warehouseExpected);
            mockRepository.Setup(repo => repo.Delete(warehouseExpected));

            var mockUnit = new Moq.Mock<IUnitOfWork>();

            var service = new WarehouseService(mockUnit.Object, mockRepository.Object);

            var warehouseResult = await service.DeleteWarehouse(warehouseId.warehouseID);

            Assert.Multiple(() =>
            {
                Assert.That(warehouseResult.Id, Is.EqualTo(warehouseExpected.Id));
                Assert.That(warehouseResult.Address, Is.EqualTo(warehouseExpected.Address.fullAddress));
                Assert.That(warehouseResult.Altitude, Is.EqualTo(warehouseExpected.Altitude.altitude));
                Assert.That(warehouseResult.Latitude, Is.EqualTo(warehouseExpected.Coordinates.latitude));
                Assert.That(warehouseResult.Longitude, Is.EqualTo(warehouseExpected.Coordinates.longitude));
                Assert.That(warehouseResult.Designation, Is.EqualTo(warehouseExpected.Designation.designation));

            });

        }

    }
}