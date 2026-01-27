# ABAP Cloud - Incident Management Project

## Project Overview

Sistema de gestión de incidencias desarrollado en ABAP Cloud siguiendo los principios de Clean Core de SAP.

## Architecture

### Technical Stack
- **Platform**: ABAP Cloud / SAP S/4HANA Cloud
- **Framework**: RAP (RESTful Application Programming)
- **Development Tools**: Eclipse ADT (ABAP Development Tools)
- **UI Framework**: Fiori Elements
- **Version Control**: Git / ABAP Git

### RAP Business Objects

#### 1. Incident (Root Entity)
- **CDS Views**:
  - `ZI_INCIDENT_ROOT` (Interface View)
  - `ZC_INCIDENT` (Consumption View)
- **Behavior**: `ZB_INCIDENT` (BDEF)
- **Implementation**: `ZBP_INCIDENT` (Behavior Pool)
- **Database Table**: `ZT_INCIDENT`

#### 2. Incident History (Composition)
- **CDS Views**:
  - `ZI_INCIDENT_HISTORY` (Interface View)
  - `ZC_INCIDENT_HISTORY` (Consumption View)
- **Behavior**: Part of Incident BDEF
- **Database Table**: `ZT_INCIDENT_HISTORY`

#### 3. Lookup Tables
- `ZT_STATUS` (Status codes)
- `ZT_PRIORITY` (Priority codes)

## Key Features

### Core Functionality
- ✅ CRUD operations for incidents
- ✅ Draft support for incident editing
- ✅ Incident history tracking
- ✅ Action: changeStatus
- ✅ Value helpers for Status and Priority
- ✅ OData V4 service binding
- ✅ Fiori Elements UI

### Clean Core Compliance
- ✅ No modifications to SAP standard code
- ✅ Custom business objects only
- ✅ Custom tables (Z_ prefix)
- ✅ Released APIs only (where applicable)
- ✅ Extension points properly used

## Development Guidelines

### Naming Conventions
```abap
" Custom Objects: Z_ prefix
ZT_INCIDENT        " Table
ZI_INCIDENT_ROOT   " Interface CDS View
ZC_INCIDENT        " Consumption CDS View
ZB_INCIDENT        " Behavior Definition
ZBP_INCIDENT       " Behavior Pool
ZSD_INCIDENT        " Service Definition
```

### Code Quality Standards
- Follow SAP ABAP Programming Guidelines
- Use ATC (ABAP Test Cockpit) checks
- Implement proper error handling
- Add comments for complex logic
- Use appropriate data types and semantics

### Clean Core Principles
1. **No Standard Code Modifications**: Never modify SAP standard code directly
2. **Use Extension Points**: Use BAdIs, CDS extensions, released APIs
3. **Maintain Upgradeability**: Design for easy upgrades
4. **Follow SAP Guidelines**: Adhere to SAP development best practices

## Project Structure

```
ABAP_CLOUD_2_PROYECTO/
├── .opencode/
│   ├── skills/                      # OpenCode skills
│   │   ├── abap-cloud-clean-core/  # Main Clean Core skill
│   │   ├── abap-core-concepts/     # Core concepts skill
│   │   ├── abap-data-modeling/     # Data modeling skill
│   │   ├── abap-behavior-definition/    # BDL skill
│   │   └── abap-behavior-implementation/ # BIL skill
│   └── opencode.json               # OpenCode configuration
├── src/
│   ├── zdd_r_inct_ymir.bdef.asbdef      # Behavior definition
│   ├── zdd_r_inct_ymir.ddls.asddls      # Interface CDS view
│   ├── zdd_c_inct_ymir.ddls.asddls      # Consumption CDS view
│   ├── zbp_dd_r_inct_ymir.clas.abap     # Behavior pool
│   ├── zbp_dd_r_inct_ymir.clas.locals_imp.abap  # Local implementation
│   └── ... (additional files)
├── docs/
│   ├── sap-concepts/          # SAP concepts documentation
│   ├── project-specific/      # Project-specific documentation
│   └── reference/            # Reference materials
│       └── sap-pdf/          # SAP PDF documentation
└── README.md                 # This file
```

## Known Issues and Solutions

### Issue: History Not Refreshing Automatically
**Problem**: When executing `changeStatus` action, the history section doesn't refresh automatically without page reload.

**Root Cause**: Side effect syntax in BDEF (line 54):
```abap
action changeStatus affects entity _History , $self;  " WRONG
```

**Solution**: Remove comma and `$self` from side effect:
```abap
action changeStatus affects entity _History;  " CORRECT
```

**Status**: ✅ Fixed in commit d6c8d26

## OpenCode Skills

### Available Skills

1. **abap-cloud-clean-core**: Main skill with Clean Core principles and RAP patterns
2. **abap-core-concepts**: Core ABAP Cloud concepts, Clean Core, extensibility
3. **abap-data-modeling**: CDS views, associations, expressions, types
4. **abap-behavior-definition**: BDL (Behavior Definition Language)
5. **abap-behavior-implementation**: BIL (Behavior Implementation Language)

### Using Skills

To load a skill in OpenCode:
```
Use the `skill` tool and specify the skill name
Example: skill({ name: "abap-data-modeling" })
```

## Development Workflow

### Local Development
1. Clone repository locally
2. Open in Eclipse ADT
3. Make changes to ABAP objects
4. Test locally
5. Commit to Git

### Deployment to SAP BTP
1. Push changes to GitHub
2. Use ABAP Git in SAP BTP
3. Pull changes from GitHub
4. Activate objects in SAP BTP
5. Test in Fiori Elements UI

## Documentation Resources

### SAP Official Documentation
- SAP Help Portal: https://help.sap.com
- ABAP Cloud Programming Guide
- RAP Development Guide
- Clean Core Documentation
- SAP API Business Hub: https://api.sap.com/

### Project Documentation
- SAP Concepts: `docs/sap-concepts/`
- Project Specific: `docs/project-specific/`
- Reference Materials: `docs/reference/`
- SAP PDFs: `docs/reference/sap-pdf/`

## Future Enhancements

### Planned Features
- [ ] Status transition validations
- [ ] Automatic history entries on any status change
- [ ] Integration with released APIs
- [ ] Advanced search functionality
- [ ] Attachment support
- [ ] Email notifications
- [ ] Performance optimizations

### Technical Debt
- [ ] Complete unit test coverage
- [ ] Implement comprehensive authorization
- [ ] Add performance monitoring
- [ ] Document all custom extensions
- [ ] Create integration tests

## Support and Maintenance

### Issue Tracking
Use GitHub Issues for bug reports and feature requests:
- Bug Reports: Label with `bug`
- Feature Requests: Label with `enhancement`
- Questions: Label with `question`

### Contact
For questions about this project:
- GitHub: https://github.com/ymirandaj/ABAP_CLOUD_2_PROYECTO
- ABAP Git: Configured in `.abapgit.xml`

---

*This project follows SAP Clean Core principles and best practices for ABAP Cloud development. Always refer to official SAP documentation for the most current information.*
