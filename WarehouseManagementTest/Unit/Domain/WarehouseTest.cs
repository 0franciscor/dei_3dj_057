
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses.ValueObjects;

namespace WarehouseManagementTest.Unit.Domain;

[TestFixture]
public class WarehouseTest
{

    [Test]
    public void CreatingAddressWithValueNull()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Address(null));
        Assert.That(ex.Message, Is.EqualTo("The address can't be null"));
    }

    [Test]
    public void CreatingAddressWithAInvalidZipCode()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Address("Rua António Bernardino,47,45-334,Porto"));
        Assert.That(ex.Message, Is.EqualTo("The zip code isn't in the right format"));
    }

    [Test]
    public void CreatingAddressWithAInvalidDoorNumber()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Address("Rua António Bernardino,-47,45375-334,Porto"));
        Assert.That(ex.Message, Is.EqualTo("The address number can't be negative"));
    }

    [Test]
    public void CreatingAddressWithAInvalidLocation()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Address("Rua António Bernardino,47,4575-334,This description is too big test should fail"));
        Assert.That(ex.Message, Is.EqualTo("The location isn't in the right format"));
    }

    [Test]
    public void CreatingAltitudeInvalid()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Altitude(-34));
        Assert.That(ex.Message, Is.EqualTo("The altitude can't be a negative number or bigger than 13000"));
    }

    [Test]
    public void CreatingLatitudeWithValueOutOfRangeAllowed()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Coordinates("999.7486º E", "167.8765º W"));
        Assert.That(ex.Message, Is.EqualTo("The latitude must be in the following format XX.XXXXº N"));
    }

    [Test]
    public void CreatingLatitudeWithNullValue()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Coordinates(null, "165"));
        Assert.That(ex.Message, Is.EqualTo("The latitude or longitude can't be null or empty"));
    }

    [Test]
    public void CreatingLatitudeWithEmptyValue()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Coordinates("", "165"));
        Assert.That(ex.Message, Is.EqualTo("The latitude or longitude can't be null or empty"));
    }

    [Test]
    public void CreatingLongitudeWithValueOutOfRangeAllowed()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Coordinates("-65.7635º N", "197.7625º S"));
        Assert.That(ex.Message, Is.EqualTo("The longitude must be in the following format XX.XXXXº W"));
    }

    [Test]
    public void CreatingLongitudeWithNullValue()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Coordinates("72.8764º N", null));
        Assert.That(ex.Message, Is.EqualTo("The latitude or longitude can't be null or empty"));
    }

    [Test]
    public void CreatingLongitudeWithEmptyValue()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Coordinates("72.6534º N", ""));
        Assert.That(ex.Message, Is.EqualTo("The latitude or longitude can't be null or empty"));
    }

    [Test]
    public void CreatingDesignationWithMoreThan50Characters()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Designation("Essa descrição é maior que a permitida pelas regras de négocio."));
        Assert.That(ex.Message, Is.EqualTo("The maximum length for description is 50 characters"));
    }

    [Test]
    public void CreatingDesignationWithNullValue()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Designation(null));
        Assert.That(ex.Message, Is.EqualTo("The designation can't be null or empty"));
    }

    [Test]
    public void CreatingDesignationWithEmptyValue()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new Designation(""));
        Assert.That(ex.Message, Is.EqualTo("The designation can't be null or empty"));
    }

    [Test]
    public void CreatingWarehouseIdWithNullValue()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new WarehouseId(null));
        Assert.That(ex.Message, Is.EqualTo("The Id can't be null or empty"));
    }

    [Test]
    public void CreatingWarehouseIdWithEmptyValue()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new WarehouseId(""));
        Assert.That(ex.Message, Is.EqualTo("The Id can't be null or empty"));
    }

    [Test]
    public void CreatingWarehouseIdWithLengthBiggerThanThree()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new WarehouseId("MG5ik"));
        Assert.That(ex.Message, Is.EqualTo("The Id must have only three characters"));
    }

    [Test]
    public void CreatingWarehouseIdWithLengthSmallerThanThree()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new WarehouseId("ik"));
        Assert.That(ex.Message, Is.EqualTo("The Id must have only three characters"));
    }

    [Test]
    public void CreatingWarehouseIdWithJustNumericCharacters()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new WarehouseId("111"));
        Assert.That(ex.Message, Is.EqualTo("The Id must be alphanumeric"));
    }

    [Test]
    public void CreatingWarehouseIdWithJustAlphabeticCharacters()
    {
        var ex = Assert.Throws<BusinessRuleValidationException>(() => new WarehouseId("CCC"));
        Assert.That(ex.Message, Is.EqualTo("The Id must be alphanumeric"));
    }


}