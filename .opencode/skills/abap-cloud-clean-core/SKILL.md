---
name: abap-cloud-clean-core
description: Guía avanzada paso a paso para desarrollo ABAP Cloud respetando principios Clean Core de SAP
---

# Skill: ABAP Cloud Development with Clean Core

## Overview
Guía paso a paso avanzada para desarrollar en ABAP Cloud respetando los principios de Clean Core de SAP.

## Prerequisites
- Experiencia avanzada en ABAP
- Conocimientos de ABAP Objects
- Fundamentos de RESTful Application Programming (RAP)
- Acceso a sistema SAP BTP o S/4HANA Cloud

## Table of Contents
1. [Clean Core Fundamentals](#1-clean-core-fundamentals)
2. [Extension Points Strategy](#2-extension-points-strategy)
3. [APIs and RAP Services](#3-apis-and-rap-services)
4. [Step-by-Step Implementation](#4-step-by-step-implementation)
5. [Best Practices and Patterns](#5-best-practices-and-patterns)

---

## 1. Clean Core Fundamentals

### 1.1 What is Clean Core?
Clean Core es el principio de mantener el núcleo estándar de SAP limpio y libre de modificaciones directas, utilizando en su lugar los mecanismos de extensión provistos.

### 1.2 Key Principles
- **No modifications to SAP standard code**
- **Use extension points only**
- **Leverage released APIs**
- **Follow SAP development guidelines**
- **Maintain upgradeability**

### 1.3 Extension Hierarchy

```
SAP Standard (Core)
├── Released APIs (C1/C2)
├── Business Add-ins (BAdIs)
├── Custom Fields and Logic (CDS Views)
├── Custom Business Objects
└── Side-by-Side Extensions (BTP)
```

---

## 2. Extension Points Strategy

### 2.1 Identify Extension Points

#### Step 1: Analyze Business Requirement
```abap
" Determine if extension is needed
" Check if standard functionality exists
" Evaluate complexity
```

#### Step 2: Search for Extension Points
Use transaction **SE18** or **SE80**:
- Business Add-ins (BAdIs)
- Enhancement Spots
- CDS Extension Views
- Release-registered APIs

#### Step 3: Evaluate Extension Type
| Requirement Type | Recommended Extension |
|-------------------|----------------------|
| Data enrichment | CDS View Extensions |
| Business logic | BAdI Implementations |
| UI enhancements | Custom CDS Views + RAP |
| New processes | Custom Business Objects |

### 2.2 BAdI Implementation Pattern

#### Finding Available BAdIs
```abap
" Use reports:
" RSBADI - Find BAdIs
" BADI_DOCU - BAdI Documentation

" Transaction codes:
" SE18 - BAdI Builder
" SE19 - BAdI Implementations
```

#### Implementing a BAdI
```abap
" 1. Create implementation class
CLASS zcl_my_extension DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_badi_interface .
    INTERFACES zif_my_badi_definition .

    METHODS:
      constructor
        IMPORTING
          iv_business_object TYPE string.

ENDCLASS.

" 2. Implement interface methods
CLASS zcl_my_extension IMPLEMENTATION.

  METHOD zif_my_badi_definition~modify_data.
    " Your enhancement logic here
    MODIFY ENTITIES OF zi_my_business_object
      ENTITY BusinessObject
        UPDATE FIELDS ( field1 field2 )
        WITH VALUE #( ( %key = iv_key
                        field1 = iv_new_value
                        field2 = iv_new_value2 ) ).
  ENDMETHOD.

ENDCLASS.
```

### 2.3 CDS View Extensions

#### Extending Standard CDS Views
```abap
" Extension View
@EndUserText.label: 'Extension for Standard CDS'
@AbapCatalog.viewEnhancementCategory: #PROJECTION_LIST
@AccessControl.authorizationCheck: #CHECK
@VDM.viewType: #COMPOSITE
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_STANDARD_CDS_EXT
  as projection on ZI_STANDARD_CDS
{
      " Key fields
  key KeyField1 as KeyField1,
  key KeyField2 as KeyField2,

      " Standard fields
  FieldA,
  FieldB,

      " Custom fields
  ZCustomField1 as CustomField1,
  ZCustomField2 as CustomField2,

      " Associations
  _Association1,
  _Association2
}
```

---

## 3. APIs and RAP Services

### 3.1 Released APIs

#### Finding Released APIs
- Transaction **API_HUB**
- SAP API Business Hub
- Package **SAP_API_* releases**

#### API Release Types
- **C1**: Stable, compatible release
- **C2**: Deprecated, still supported
- **D**: Development purpose only

### 3.2 Creating RAP Business Objects

#### Step 1: Define CDS Interface View
```abap
@EndUserText.label: 'Interface View for Custom BO'
@AccessControl.authorizationCheck: #CHECK
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: { serviceQuality: #X, sizeCategory: #M, dataCategory: #MIXED }
define root view entity ZI_CUSTOM_BO
  as select from zcustom_table as Custom
{
  key custom_id            as CustomId,
      custom_description   as CustomDescription,
      custom_status        as CustomStatus,
      created_at           as CreatedAt,
      created_by           as CreatedBy,
      last_changed_at      as LastChangedAt,
      last_changed_by      as LastChangedBy,

      " Associations
      _CustomItems         as Items
}
```

#### Step 2: Define CDS Consumption View
```abap
@EndUserText.label: 'Consumption View for Custom BO'
@Metadata.allowExtensions: true
@ObjectModel.usageType: { serviceQuality: #X, sizeCategory: #M, dataCategory: #MIXED }
define root view entity ZC_CUSTOM_BO
  provider contract transactional_query
  as projection on ZI_CUSTOM_BO
{
  key CustomId,
      CustomDescription,
      CustomStatus,
      CreatedAt,
      CreatedBy,
      LastChangedAt,
      LastChangedBy,

      " Virtual fields
      @ObjectModel.readOnly: true
      @Semantics.amount.currencyCode: 'CurrencyCode'
      TotalAmount,

      CurrencyCode,

      " Actions
      @ObjectModel.actionFeature: true
      approve,
      @ObjectModel.actionFeature: true
      reject,

      " Associations
      Items
}
```

#### Step 3: Define Behavior Definition
```abap
managed; // use implementation class

define behavior for ZC_CUSTOM_BO
persistent table zcustom_table
lock master
authorization master ( instance )

// Create, Read, Update, Delete
create;
update;
delete;

// Field properties
field ( readonly ) CustomId, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy;
field ( mandatory ) CustomDescription;

// Determinations
determination validateStatus on modify { CustomStatus }
determination setCreatedAt on save { create }
determination setChangedAt on save { update }

// Validations
validation validateStatus on save { create; update; CustomStatus }

// Actions
action ( features : instance ) approve result [1] $self;
action ( features : instance ) reject result [1] $self;

// Associations
association Items { create; }
```

#### Step 4: Implement Behavior Class
```abap
CLASS lhc_custom_bo DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    " Validation methods
    METHODS validatestatus FOR VALIDATE ON SAVE
      IMPORTING keys FOR custombo~ValidateStatus.

    " Determination methods
    METHODS setcreatedat FOR DETERMINE ON SAVE
      IMPORTING keys FOR custombo~SetCreatedAt.

    " Action methods
    METHODS approve FOR MODIFY
      IMPORTING keys FOR ACTION custombo~approve RESULT result.

    METHODS reject FOR MODIFY
      IMPORTING keys FOR ACTION custombo~reject RESULT result.

ENDCLASS.

CLASS lhc_custom_bo IMPLEMENTATION.

  METHOD validatestatus.
    " Validation logic
    DATA(lt_entities) = READ ENTITIES OF zc_custom_bo IN LOCAL MODE
      ENTITY CustomBO
        FIELDS ( CustomStatus )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_data).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<fs_data>).
      IF <fs_data>-CustomStatus NOT IN 'APPR' AND
         <fs_data>-CustomStatus NOT IN 'PEND'.
        APPEND VALUE #( %tky = <fs_data>-%tky ) TO failed-custombo.
        APPEND VALUE #( %tky = <fs_data>-%tky
                        %msg = new_message( id       = 'ZMY_MESSAGES'
                                            number   = '001'
                                            severity = if_abap_behv_message=>severity-error
                                            v1       = <fs_data>-CustomStatus )
                        %element-CustomStatus = if_abap_behv=>mk-on )
              TO reported-custombo.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD setcreatedat.
    MODIFY ENTITIES OF zc_custom_bo IN LOCAL MODE
      ENTITY CustomBO
        UPDATE FIELDS ( CreatedAt CreatedBy )
        WITH VALUE #( FOR key IN keys
                      ( %tky = key-%tky
                        CreatedAt = cl_abap_context_info=>get_system_time( )
                        CreatedBy = cl_abap_context_info=>get_user_technical_name( ) ) ).
  ENDMETHOD.

  METHOD approve.
    MODIFY ENTITIES OF zc_custom_bo IN LOCAL MODE
      ENTITY CustomBO
        UPDATE FIELDS ( CustomStatus LastChangedAt LastChangedBy )
        WITH VALUE #( FOR key IN keys
                      ( %tky = key-%tky
                        CustomStatus = 'APPR'
                        LastChangedAt = cl_abap_context_info=>get_system_time( )
                        LastChangedBy = cl_abap_context_info=>get_user_technical_name( ) ) ).

    READ ENTITIES OF zc_custom_bo IN LOCAL MODE
      ENTITY CustomBO
        FIELDS ( CustomId CustomDescription )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_data).

    result = VALUE #( FOR row IN lt_data
                       ( %tky = row-%tky
                         %param = row ) ).
  ENDMETHOD.

  METHOD reject.
    MODIFY ENTITIES OF zc_custom_bo IN LOCAL MODE
      ENTITY CustomBO
        UPDATE FIELDS ( CustomStatus LastChangedAt LastChangedBy )
        WITH VALUE #( FOR key IN keys
                      ( %tky = key-%tky
                        CustomStatus = 'REJC'
                        LastChangedAt = cl_abap_context_info=>get_system_time( )
                        LastChangedBy = cl_abap_context_info=>get_user_technical_name( ) ) ).

    READ ENTITIES OF zc_custom_bo IN LOCAL MODE
      ENTITY CustomBO
        FIELDS ( CustomId CustomDescription )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_data).

    result = VALUE #( FOR row IN lt_data
                       ( %tky = row-%tky
                         %param = row ) ).
  ENDMETHOD.

ENDCLASS.
```

#### Step 5: Define Service Binding
```abap
" Service Definition
@EndUserText.label: 'Service Definition for Custom BO'
define service ZSD_CUSTOM_BO {
  expose ZC_CUSTOM_BO as CustomBO;
  expose ZC_CUSTOM_BO_ITEMS as Items;
}

" Service Binding
" Create in transaction SEGW or ADT
" Choose protocol: OData V4 (recommended)
" Assign service definition
```

---

## 4. Step-by-Step Implementation

### Step 1: Requirement Analysis
```
1. Document business requirement
2. Identify standard objects involved
3. Check for existing functionality
4. Determine extension strategy
```

### Step 2: Extension Point Discovery
```
1. Search for BAdIs (SE18)
2. Check for CDS extensions
3. Review released APIs (API_HUB)
4. Evaluate extensibility options
```

### Step 3: Design Extension
```
1. Create design document
2. Define data model extensions
3. Plan business logic
4. Define authorization concept
```

### Step 4: Implementation
```
1. Create CDS extensions
2. Implement BAdIs if needed
3. Create RAP services
4. Implement behavior classes
5. Create service bindings
```

### Step 5: Testing
```
1. Unit tests (ATC)
2. Integration tests
3. User acceptance tests
4. Performance tests
5. Security checks
```

### Step 6: Deployment
```
1. Code review
2. Transport to QA
3. Final testing in QA
4. Transport to Production
5. Monitor post-deployment
```

---

## 5. Best Practices and Patterns

### 5.1 Naming Conventions
```abap
" Custom objects prefix: Z or Y
" CDS Views: ZI_* (Interface), ZC_* (Consumption)
" Behavior classes: ZCL_* (Handler), ZBP_* (Behavior Pool)
" Service definitions: ZSD_*
" Tables: Z* or Y*
" BAdI implementations: ZIMP_*
```

### 5.2 Code Quality
- Use ATC (ABAP Test Cockpit) checks
- Follow ABAP Programming Guidelines
- Implement error handling
- Add proper comments for complex logic
- Use appropriate data types

### 5.3 Performance
- Use buffer options for CDS views
- Implement proper joins
- Avoid nested selects
- Use field symbols for internal table operations
- Optimize database access

### 5.4 Security
- Implement role-based authorization
- Use ABAP auth checks
- Validate input data
- Implement CSRF protection for services
- Use secure coding practices

### 5.5 Migration and Upgradeability
- Document all extensions
- Keep extensions separate from standard
- Use released APIs only
- Test upgrades in sandbox
- Monitor deprecation notices

### 5.6 Common Patterns

#### Pattern 1: Data Enrichment
```abap
" Use CDS view extension to add custom fields
@AbapCatalog.viewEnhancementCategory: #PROJECTION_LIST
define view entity ZI_STANDARD_EXT
  as projection on ZI_STANDARD
{
  KeyField,
  StandardField,
  ZCustomField,  " New custom field
  _CustomAssociation
}
```

#### Pattern 2: Business Logic Enhancement
```abap
" Implement BAdI to add logic
CLASS zcl_standard_badi_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_badi_interface .
    INTERFACES zif_standard_badi.

ENDCLASS.

CLASS zcl_standard_badi_impl IMPLEMENTATION.

  METHOD zif_standard_badi~custom_check.
    " Add custom validation logic
    IF iv_value < '100'.
      RAISE EXCEPTION TYPE zcx_custom_error
        EXPORTING
          textid = zcx_custom_error=>invalid_value.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
```

#### Pattern 3: Side-by-Side Extension
```abap
" Create separate business object in BTP
" Integrate via APIs
" Use SAP BTP Cloud Foundry
" Deploy SAP CAP or custom microservice
```

---

## Additional Resources

### Documentation
- SAP Help Portal: https://help.sap.com
- ABAP Programming Guidelines
- RAP Development Guide
- Clean Core Documentation

### Tools
- ADT (ABAP Development Tools) for Eclipse
- SE80, SE18, SE19 transactions
- API_HUB
- ATC (ABAP Test Cockpit)

### Community
- SAP Community
- SAP Developer Center
- Stack Overflow - ABAP tag

---

## Checklist for Clean Core Development

- [ ] Standard code not modified
- [ ] Extension points properly identified
- [ ] Released APIs used
- [ ] Proper naming conventions followed
- [ ] ATC checks passed
- [ ] Authorization implemented
- [ ] Error handling in place
- [ ] Documentation complete
- [ ] Testing completed
- [ ] Transport documented

---

## Common Pitfalls to Avoid

1. **Modifying standard code** - Always use extension points
2. **Using internal APIs** - Use only released APIs
3. **Skipping ATC checks** - Essential for code quality
4. **Ignoring upgrade impact** - Consider future upgrades
5. **Hardcoding values** - Use configuration tables
6. **Poor error handling** - Implement proper exceptions
7. **Missing authorization** - Implement role checks
8. **Insufficient testing** - Test all scenarios
9. **No documentation** - Document all changes
10. **Direct database access** - Use proper APIs

---

## Troubleshooting

### Issue: Extension not working
```
1. Check BAdI implementation is active
2. Verify filter values if using filters
3. Check implementation class errors
4. Review activation status
```

### Issue: CDS view not accessible
```
1. Check authorization
2. Verify view is activated
3. Check DDL source
4. Review enhancement category
```

### Issue: RAP service errors
```
1. Check behavior definition syntax
2. Verify behavior class implementation
3. Check service binding
4. Review metadata
```

---

## Next Steps

After mastering Clean Core development:
1. Explore advanced RAP features
2. Learn SAP BTP integration
3. Study SAP Fiori Elements integration
4. Consider SAP CAP for side-by-side extensions
5. Stay updated with SAP releases and deprecations

---

*This skill provides a comprehensive foundation for developing ABAP Cloud applications while maintaining Clean Core principles. Always refer to official SAP documentation for the most current information.*
