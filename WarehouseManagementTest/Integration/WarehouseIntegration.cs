using EletricGo.Controllers;
using EletricGo.Domain.Cities;
using EletricGo.Domain.Cities.ValueObjects;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses;
using EletricGo.Domain.Warehouses.DTO;
using EletricGo.Domain.Warehouses.ValueObjects;
using EletricGo.Services;
using EletricGo.Services.Interfaces;
using Microsoft.AspNetCore.Mvc;
using Moq;
using static System.Console;

namespace WarehouseManagementTest.Integration;

[TestFixture]
internal class WarehouseIntegration
{
    private readonly string? id = "WH";

    private readonly string address = "Rua António Bernardino,47,4535-334,Porto";

    private readonly int altitude = 200;

    private readonly string latitude = "45.6457º N";

    private readonly string longitude = "79.7364º W";

    private readonly string description = "Porto";
    
    private readonly string CityId = "8";

    //SETUP LISTS FOR GETALL()
    private List<Warehouse> GetWarehousesList()
    {
        var warehouseList = new List<Warehouse>
        {
            new Warehouse(new  WarehouseId(id + "1"), new Address(address), new Altitude(altitude), new Coordinates(latitude,longitude), new Designation(description), new CityId(CityId)),
            new Warehouse(new  WarehouseId(id + "2"), new Address(address), new Altitude(altitude), new Coordinates(latitude,longitude), new Designation(description), new CityId(CityId)),
            new Warehouse(new  WarehouseId(id + "3"), new Address(address), new Altitude(altitude), new Coordinates(latitude,longitude), new Designation(description), new CityId(CityId)),
            new Warehouse(new  WarehouseId(id + "4"), new Address(address), new Altitude(altitude), new Coordinates(latitude,longitude), new Designation(description), new CityId(CityId))

        };

        return warehouseList;
    }

    [Test]
    public void DefineDriverServiceConstructor()
    {
        var mockRepository = new Mock<IWarehouseRepository>();
        var mockUnit = new Mock<IUnitOfWork>();
        var cityService = new Moq.Mock<ICityService>();

        var service = new WarehouseService(mockUnit.Object, mockRepository.Object, cityService.Object);

        Assert.That(service, Is.Not.Null);
    }

    [Test]
    public async Task GetAllTest()
    {
        var expectedList = GetWarehousesList();

        var mockUnit = new Mock<IUnitOfWork>();
        var mockRepository = new Mock<IWarehouseRepository>();
        mockRepository.Setup(repo => repo.GetAll()).ReturnsAsync(expectedList);
        
        var cityService = new CityService(mockUnit.Object, new Mock<ICityRepository>().Object);

        var service = new WarehouseService(mockUnit.Object, mockRepository.Object, cityService);
        var controller = new WarehouseController(service);

        var resultList = await controller.Get();

        for (var i = 0; i < expectedList.Count; i++)
        {

            Assert.That(resultList.Value[i].Id, Is.EqualTo(expectedList[i].Id));
        }
    }

    [Test]
    public async Task GetByIDTest()
    {
        var warehouse = new Warehouse(new WarehouseId("WH1"), new Address(address),
            new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description), new CityId(CityId));
        var warehouseExpected = warehouse.ToWarehouseDto();
        var warehouseId = new WarehouseId("WH1");

        var mockUnit = new Mock<IUnitOfWork>();
        var mockRepository = new Mock<IWarehouseRepository>();
        mockRepository.Setup(repo => repo.GetByID(warehouseId)).ReturnsAsync(warehouse);
        
        var cityService = new CityService(mockUnit.Object, new Mock<ICityRepository>().Object);
        var service = new WarehouseService(mockUnit.Object, mockRepository.Object, cityService);

        var controller = new WarehouseController(service);

        var aux = await controller.GetByID("WH1");

        if (aux == null)
            Assert.Fail();
        else
        {
            var warehouseResult = (WarehouseDto)(aux.Result as OkObjectResult).Value;

            Assert.That(warehouseResult.Id, Is.EqualTo(warehouseExpected.Id));

        }
    }


    [Test]
    public async Task PostTest()
    {
        var warehouse = new Warehouse(new WarehouseId(id + "1"), new Address(address),
            new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description), new CityId(CityId));
        var warehouseExpected = warehouse.ToWarehouseDto();

        var mockUnit = new Mock<IUnitOfWork>();
        var mockRepository = new Mock<IWarehouseRepository>();


        var mockRepo = new Mock<IWarehouseRepository>();
        mockRepo.Setup(repo => repo.Add(warehouse)).ReturnsAsync(warehouse);
        mockRepository.Setup(repo => repo.GetByID(new WarehouseId(id + "1"))).ReturnsAsync(null as Warehouse);
        var mockUnitRepo = new Mock<IUnitOfWork>();
        mockUnitRepo.Setup(repo => repo.CommitAsync());
        
        var cityService = new CityService(mockUnit.Object, new Mock<ICityRepository>().Object);
        var service = new WarehouseService(mockUnitRepo.Object, mockRepo.Object, cityService);
        var controller = new WarehouseController(service);

        var aux = await controller.Post(warehouseExpected);

        if (aux == null)
            Assert.Fail();
        else
        {
            var warehouseResult = (WarehouseDto)(aux.Result as CreatedAtActionResult).Value;

            Assert.That(warehouseResult.Id, Is.EqualTo(warehouseExpected.Id));
        }
    }

    [Test]
    public async Task PutTest()
    {
        var warehouse = new Warehouse(new WarehouseId(id + "1"), new Address(address),
            new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description), new CityId(CityId));

        var preChangeDto = warehouse.ToWarehouseDto();
        var postChangeDto = warehouse.ToWarehouseDto();

        int newAltitude = 237;
        postChangeDto.Altitude = newAltitude;

        var mockRepository = new Mock<IWarehouseRepository>();
        mockRepository.Setup(repo => repo.GetByID(new WarehouseId(id + "1"))).ReturnsAsync(warehouse);

        var mockUnit = new Mock<IUnitOfWork>();
        mockUnit.Setup(repo => repo.CommitAsync()); //does not return, but the service method updates the object
        
        var cityService = new CityService(mockUnit.Object, new Mock<ICityRepository>().Object);
        var service = new WarehouseService(mockUnit.Object, mockRepository.Object, cityService);
        
        var controller = new WarehouseController(service);

        var aux = await controller.Put(postChangeDto);

        if (aux == null)
            Assert.Fail();

        else
        {
            var warehouseResult = (WarehouseDto)(aux.Result as OkObjectResult).Value;

            Assert.That(warehouseResult.Altitude, Is.EqualTo(newAltitude));
        }
    }

    [Test]
    public async Task DeleteTest()
    {
        var warehouse = new Warehouse(new WarehouseId(id + "1"), new Address(address),
            new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description), new CityId(CityId));
        var warehouseExpected = warehouse.ToWarehouseDto();

        var mockRepository = new Mock<IWarehouseRepository>();
        mockRepository.Setup(repo => repo.GetByID(new WarehouseId(id + "1"))).ReturnsAsync(warehouse);
        mockRepository.Setup(repo => repo.Delete(warehouse));
        var mockUnit = new Mock<IUnitOfWork>();
        
        var cityService = new CityService(mockUnit.Object, new Mock<ICityRepository>().Object);

        var service = new WarehouseService(mockUnit.Object, mockRepository.Object, cityService);
        var controller = new WarehouseController(service);
        var aux = await controller.Delete("WH1");

        if (aux == null)
            Assert.Fail();
        else
        {
            var warehouseresult = (WarehouseDto)(aux.Result as OkObjectResult).Value;

            Assert.AreEqual(warehouseresult.Id, "WH1");
        }
    }

}