  METHOD divisionset_get_entityset.


    DATA ls_entityset   LIKE LINE OF et_entityset.



    DATA lt_org_hierarchy TYPE TABLE OF struc.

    DATA lt_org_names TYPE TABLE OF objec.

    TYPES: BEGIN OF ty_struct_and_rating_tt,
             level        TYPE int4,
             objid        TYPE realo,
             stext        TYPE stext,
             avgrating    TYPE float,
             openitems    TYPE /iwbep/sb_odata_ty_int2,
             rating1count TYPE /iwbep/sb_odata_ty_int2,
             rating2count TYPE /iwbep/sb_odata_ty_int2,
             rating3count TYPE /iwbep/sb_odata_ty_int2,
             rating4count TYPE /iwbep/sb_odata_ty_int2,
             rating5count TYPE /iwbep/sb_odata_ty_int2,
           END OF ty_struct_and_rating_tt.

    DATA lt_struct_and_rating TYPE STANDARD TABLE OF ty_struct_and_rating_tt.

    DATA ls_struct_and_rating LIKE LINE OF lt_struct_and_rating.

    FIELD-SYMBOLS <ls_org_hierarchy> LIKE LINE OF lt_org_hierarchy.
    FIELD-SYMBOLS <ls_org_names> LIKE LINE OF lt_org_names.

    DATA lv_processor_bp TYPE bu_partner.
    DATA lv_employer_bp TYPE bu_partner.
    DATA lv_employer_org_unit TYPE realo.


    TYPES: BEGIN OF ty_processor_units_tt,
             objid     TYPE realo,
             rating    TYPE int4,
             openitems TYPE /iwbep/sb_odata_ty_int2,
           END OF ty_processor_units_tt.

    DATA lt_processor_units TYPE STANDARD TABLE OF ty_processor_units_tt.
    FIELD-SYMBOLS <ls_processor_units> LIKE LINE OF lt_processor_units.

    DATA ls_processor_units LIKE LINE OF lt_processor_units.

    DATA lv_processor_unit_pos TYPE int4.
    DATA lv_loop_idx TYPE int4.

    DATA lv_unit_level TYPE int4.

    DATA lv_incident_rating TYPE numc3.
    DATA lv_incident_rating_random TYPE int4.


    DATA:
      api_object    TYPE REF TO cl_ags_crm_1o_api,
      api_object_sd TYPE REF TO cl_ags_crm_1o_api_sd,
      et_partner    TYPE crmt_partner_external_wrkt,
      ls_partner    TYPE crmt_partner_external_wrk.


    TYPES: BEGIN OF ty_open_requests_tt,
             guid      TYPE crmt_object_guid,
             object_id TYPE crmt_object_id_db,
             rating    TYPE numc3,
           END OF ty_open_requests_tt.

    DATA lt_open_requests TYPE STANDARD TABLE OF ty_open_requests_tt.
    DATA lv_rating_first_date TYPE dats.
    DATA lv_user_status TYPE crm_j_status.
    FIELD-SYMBOLS <ls_open_request> LIKE LINE OF lt_open_requests.


    DATA lv_rating1count TYPE /iwbep/sb_odata_ty_int2.
    DATA lv_rating2count TYPE /iwbep/sb_odata_ty_int2.
    DATA lv_rating3count TYPE /iwbep/sb_odata_ty_int2.
    DATA lv_rating4count TYPE /iwbep/sb_odata_ty_int2.
    DATA lv_rating5count TYPE /iwbep/sb_odata_ty_int2.

    DATA lv_openitems TYPE /iwbep/sb_odata_ty_int2.


    TYPES: BEGIN OF ty_excluded_records_tt,
             orgunit TYPE char12,
           END OF ty_excluded_records_tt.

    DATA lt_excluded_records TYPE STANDARD TABLE OF ty_excluded_records_tt.
    DATA ls_excluded_records LIKE LINE OF lt_excluded_records.

    DATA: lv_datefrom TYPE dats,
          lv_dateto   TYPE dats.

    DATA lv_process_type TYPE crmt_process_type_db.
    DATA lv_root_org_unit TYPE realo.
    DATA lv_demo_mode TYPE char1.


    " Enabling filtering
    DATA(it_filter_so) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    IF line_exists( it_filter_so[ property = 'FUNCTION' ] ).

      DATA(rg_function_so) = it_filter_so[ property = 'FUNCTION' ]-select_options.

    ENDIF.


    IF line_exists( it_filter_so[ property = 'DATEFROM' ] ).

      DATA(it_datefrom_struct) = it_filter_so[ property = 'DATEFROM' ]-select_options.

      LOOP AT it_datefrom_struct ASSIGNING FIELD-SYMBOL(<it_datefrom_struct>).
        lv_datefrom = <it_datefrom_struct>-low.
      ENDLOOP.

    ENDIF.


    IF line_exists( it_filter_so[ property = 'DATETO' ] ).

      DATA(it_dateto_struct) = it_filter_so[ property = 'DATETO' ]-select_options.

      LOOP AT it_dateto_struct ASSIGNING FIELD-SYMBOL(<it_dateto_struct>).
        lv_dateto = <it_dateto_struct>-low.
      ENDLOOP.

    ENDIF.


    IF  ( lv_datefrom IS  INITIAL ) AND ( lv_dateto IS INITIAL ).

      DATA lv_year TYPE n LENGTH 4.
      lv_year = sy-datum+0(4).
      lv_year = lv_year - 2.
      CONCATENATE lv_year '01' '01' INTO lv_datefrom.
      lv_dateto = sy-datum.

    ENDIF.

    " Picking up root org unit node from parameters table

    SELECT SINGLE value FROM zcasemgmtparam INTO lv_root_org_unit
      WHERE param = 'REP_ROOT_ORG_UNIT'.

    " Receiving whole org structure of the system

    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype      = 'O'
        act_objid      = lv_root_org_unit
        act_wegid      = 'B002'
      TABLES
        result_struc   = lt_org_hierarchy
        result_objec   = lt_org_names
      EXCEPTIONS
        no_plvar_found = 1
        no_entry_found = 2
        OTHERS         = 3.

    lv_rating_first_date = sy-datum - 900.

    IF  ( lv_datefrom IS NOT INITIAL ) AND ( lv_dateto IS NOT INITIAL ).

      " Picking up process type for reporting from parameters table

      SELECT SINGLE value FROM zcasemgmtparam INTO lv_process_type
             WHERE param = 'REP_PROC_TYPE'.

      SELECT guid object_id  FROM crmd_orderadm_h INTO TABLE lt_open_requests
             WHERE process_type = lv_process_type AND posting_date GE lv_datefrom AND posting_date LE lv_dateto.

    ENDIF. " IF  ( lv_datefrom IS NOT INITIAL ) AND ( lv_dateto IS NOT INITIAL )

    "AND posting_date > lv_rating_first_date.

    LOOP AT lt_open_requests ASSIGNING <ls_open_request>.

      CLEAR et_partner.
      CLEAR ls_partner.
      CLEAR lv_employer_bp.
      CLEAR lv_employer_org_unit.
      CLEAR lv_user_status.
      CLEAR lv_processor_bp.
      CLEAR lv_incident_rating.
      CLEAR lv_processor_unit_pos.
      CLEAR lv_unit_level.
      CLEAR ls_processor_units.

      CALL METHOD cl_ags_crm_1o_api=>get_instance
        EXPORTING
          iv_header_guid                = <ls_open_request>-guid
          iv_process_mode               = 'C'
          iv_process_type               = lv_process_type
        IMPORTING
          eo_instance                   = api_object
        EXCEPTIONS
          invalid_parameter_combination = 1
          error_occurred                = 2
          OTHERS                        = 3.

      " Searching for request status

      api_object->get_status( IMPORTING ev_user_status = lv_user_status ).


      " Processing closed incidents

      api_object_sd ?= api_object.
      api_object_sd->get_partners( IMPORTING et_partner = et_partner ).

      " Picking up processor SLFN0004 from partners

      LOOP AT et_partner INTO ls_partner.
        IF ls_partner-ref_partner_fct = 'SLFN0004'.
          lv_processor_bp = ls_partner-partner_no.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_processor_bp
            IMPORTING
              output = lv_processor_bp.

          " Search to which org structure processor belongs to

          " Receiving BP for processor support team

          SELECT SINGLE partner1 INTO lv_employer_bp FROM but050 WHERE partner2 = lv_processor_bp AND reltyp = 'BUR010'.

        ENDIF. " IF ls_partner-ref_partner_fct = 'SLFN0004'
      ENDLOOP. " LOOP AT et_partner INTO ls_partner

      " Processor cannot be found so we're taking support team SLFN0003 then

      IF lv_employer_bp IS INITIAL.
        LOOP AT et_partner INTO ls_partner.
          IF ls_partner-ref_partner_fct = 'SLFN0003'.
            lv_employer_bp = ls_partner-partner_no.

          ENDIF. " IF ls_partner-ref_partner_fct = 'SLFN0003'
        ENDLOOP. " LOOP AT et_partner INTO ls_partner

      ENDIF. " IF lv_processor_bp IS NOT INITIAL

      IF lv_employer_bp IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_employer_bp
          IMPORTING
            output = lv_employer_bp.

        " Receiving org structure id for processor
        SELECT SINGLE objid INTO lv_employer_org_unit FROM hrp1001 WHERE sobid = lv_employer_bp AND sclas = 'BP'.

        IF lv_employer_org_unit IS NOT INITIAL.

          " Checking if we are in demo mode

          SELECT SINGLE value FROM zcasemgmtparam INTO lv_demo_mode
           WHERE param = 'REP_DEMO_MODE'.

          IF ( lv_demo_mode = 'X' ).

            CALL FUNCTION 'QF05_RANDOM_INTEGER'
              EXPORTING
                ran_int_max   = 5
                ran_int_min   = 1
              IMPORTING
                ran_int       = lv_incident_rating_random
              EXCEPTIONS
                invalid_input = 1
                OTHERS        = 2.

            lv_incident_rating = lv_incident_rating_random.

          ELSE.

            SELECT SINGLE zzfld000000 FROM crmd_customer_h INTO lv_incident_rating WHERE guid = <ls_open_request>-guid.

          ENDIF. " IF ( lv_demo_mode = 'X' )

          IF sy-subrc <> 0.
            lv_incident_rating = 0.
          ENDIF. " IF sy-subrc = 0

          " Record all org units that correspond to processor

          " First recording the dedicated unit (direct employement)

          ls_processor_units-objid = lv_employer_org_unit.

          "ls_processor_units-object_id = <ls_open_request>-object_id.

          IF lv_user_status = 'E0008'.
            ls_processor_units-rating = lv_incident_rating.
          ELSE. "  IF lv_user_status = 'E0008'.
            ls_processor_units-openitems = 1.
          ENDIF. "  IF lv_user_status = 'E0008'.




          APPEND ls_processor_units TO lt_processor_units.

          " Receiving a position and level of processor's org unit

          READ TABLE lt_org_hierarchy ASSIGNING <ls_org_hierarchy> WITH KEY objid = lv_employer_org_unit.

          IF ( <ls_org_hierarchy> IS ASSIGNED ) AND ( sy-subrc = 0 ).

            lv_processor_unit_pos = sy-tabix.
            lv_unit_level = <ls_org_hierarchy>-level.

            " Looping through org units backwards to get all other units from heirarchy

            lv_unit_level = lv_unit_level - 1.

            DO lv_processor_unit_pos TIMES.

              CLEAR  ls_processor_units.

              " Switching to one level up
              lv_processor_unit_pos = lv_processor_unit_pos - 1.

              UNASSIGN <ls_org_hierarchy>.

              READ TABLE lt_org_hierarchy ASSIGNING <ls_org_hierarchy> INDEX  lv_processor_unit_pos .

              IF ( <ls_org_hierarchy> IS ASSIGNED ) AND ( <ls_org_hierarchy>-level = lv_unit_level ).

                ls_processor_units-objid = <ls_org_hierarchy>-objid.


                IF lv_user_status = 'E0008'.
                  ls_processor_units-rating = lv_incident_rating.
                ELSE. "  IF lv_user_status = 'E0008'.
                  ls_processor_units-openitems = 1.
                ENDIF.

                APPEND ls_processor_units TO lt_processor_units.
                lv_unit_level = lv_unit_level - 1.

              ELSE.
                CONTINUE.
              ENDIF. " IF ( <ls_org_hierarchy>-level = lv_unit_level )

            ENDDO. " DO lv_processor_unit_pos TIMES

          ENDIF. " IF ( <ls_org_hierarchy> IS ASSIGNED ) AND ( sy-subrc = 0 )



        ENDIF. " IF lv_employer_org_unit IS NOT INITIAL

      ENDIF. "  IF lv_employer_bp IS NOT INITIAL.



    ENDLOOP. " loop at lt_open_requests assigning <ls_open_request>


    " Looping through org structure

    LOOP AT lt_org_hierarchy ASSIGNING <ls_org_hierarchy>.

      CLEAR ls_struct_and_rating.
      UNASSIGN <ls_org_names>.

      lv_rating1count = 0.
      lv_rating2count = 0.
      lv_rating3count = 0.
      lv_rating4count = 0.
      lv_rating5count = 0.
      lv_openitems = 0.
*
*  ls_struct_and_rating-level = <ls_org_hierarchy>-level.
*  ls_struct_and_rating-objid = <ls_org_hierarchy>-objid.
*
      ls_entityset-hierarchy = <ls_org_hierarchy>-level.



      CONDENSE ls_entityset-hierarchy.
      ls_entityset-orgunit = <ls_org_hierarchy>-objid.

      " Picking up a name of org unit

      READ TABLE lt_org_names WITH KEY objid = <ls_org_hierarchy>-objid ASSIGNING <ls_org_names>.

      IF ( <ls_org_names> IS ASSIGNED ).

        "ls_struct_and_rating-stext = <ls_org_names>-stext.

        ls_entityset-orgunitname = <ls_org_names>-stext.

        " Filling function

        CASE ls_entityset-hierarchy.
          WHEN 2.
            ls_entityset-function = <ls_org_names>-stext.
          WHEN OTHERS.
            ls_entityset-function = 'null'.

        ENDCASE. " CASE ls_entityset-hierarchy

      ENDIF. " IF ( <ls_org_names> IS ASSIGNED )

      " Checking if we should update org unit statistics with incident rating

      LOOP AT lt_processor_units ASSIGNING <ls_processor_units>.

        IF ( <ls_processor_units>-objid = <ls_org_hierarchy>-objid ).

          CASE <ls_processor_units>-rating.
            WHEN 1.
              lv_rating1count = lv_rating1count + 1.
            WHEN 2.
              lv_rating2count = lv_rating2count + 1.
            WHEN 3.
              lv_rating3count = lv_rating3count + 1.
            WHEN 4.
              lv_rating4count = lv_rating4count + 1.
            WHEN 5.
              lv_rating5count = lv_rating5count + 1.

          ENDCASE. " CASE <ls_processor_units>-rating

          IF ( <ls_processor_units>-openitems > 0 ).
            lv_openitems = lv_openitems + 1.
          ENDIF.  " if <ls_processor_units>-openitems > 0


        ENDIF. " IF ( <ls_processor_units>-objid = <ls_org_hierarchy>-objid )

      ENDLOOP. " LOOP AT lt_processor_units ASSIGNING <ls_processor_units>
*
*  ls_struct_and_rating-rating1count = lv_rating1count.
*  ls_struct_and_rating-rating2count = lv_rating2count.
*  ls_struct_and_rating-rating3count = lv_rating3count.
*  ls_struct_and_rating-rating4count = lv_rating4count.
*  ls_struct_and_rating-rating5count = lv_rating5count.
*  ls_struct_and_rating-openitems = lv_openitems.

      ls_entityset-rating1count = lv_rating1count.
      ls_entityset-rating2count = lv_rating2count.
      ls_entityset-rating3count = lv_rating3count.
      ls_entityset-rating4count = lv_rating4count.
      ls_entityset-rating5count = lv_rating5count.
      ls_entityset-openitems = lv_openitems.
      ls_entityset-datefrom = lv_datefrom.
      ls_entityset-dateto = lv_dateto.


      " Calculating average rating
*
*  ls_struct_and_rating-avgrating =
*    ( ( ls_struct_and_rating-rating1count * 1 ) +
*      ( ls_struct_and_rating-rating2count * 2 ) +
*        ( ls_struct_and_rating-rating3count * 3 ) +
*            ( ls_struct_and_rating-rating4count * 4 ) +
*              ( ls_struct_and_rating-rating5count * 5 ) ) /
*              ( ls_struct_and_rating-rating1count + ls_struct_and_rating-rating2count + ls_struct_and_rating-rating3count +
*                ls_struct_and_rating-rating4count + ls_struct_and_rating-rating5count ).
*
      ls_entityset-avgrating =
      ( ( ls_entityset-rating1count * 1 ) +
        ( ls_entityset-rating2count * 2 ) +
          ( ls_entityset-rating3count * 3 ) +
              ( ls_entityset-rating4count * 4 ) +
                ( ls_entityset-rating5count * 5 ) ) /
                ( ls_entityset-rating1count + ls_entityset-rating2count + ls_entityset-rating3count +
                  ls_entityset-rating4count + ls_entityset-rating5count ).


      "APPEND ls_struct_and_rating TO lt_struct_and_rating.

      APPEND ls_entityset TO et_entityset.
      CLEAR ls_entityset.


    ENDLOOP. "  LOOP AT lt_org_hierarchy ASSIGNING <ls_org_hierarchy>

    " Removing records by filter

    IF rg_function_so IS NOT INITIAL.

      LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset_filter>)
         WHERE orgunit NOT IN rg_function_so.

        CLEAR ls_excluded_records.
        ls_excluded_records-orgunit =  <ls_entityset_filter>-orgunit.
        APPEND ls_excluded_records TO lt_excluded_records.

      ENDLOOP.


      IF ( lt_excluded_records IS NOT INITIAL ).

        LOOP AT lt_excluded_records ASSIGNING FIELD-SYMBOL(<ls_excluded_records>).

          DELETE et_entityset WHERE orgunit = <ls_excluded_records>-orgunit.

        ENDLOOP. " LOOP AT lt_excluded_records ASSIGNING FIELD-SYMBOL(<ls_excluded_records>).

      ENDIF. "IF ( lt_excluded_records IS NOT INITIAL )

    ENDIF." IF rg_matnr_so IS NOT INITIAL.


  ENDMETHOD.