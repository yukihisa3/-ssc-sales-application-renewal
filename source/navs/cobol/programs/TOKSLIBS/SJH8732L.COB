****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　受領件数リスト　*
*    モジュール名　　　　：　ＤＣＭＪＡＰＡＮ　受領件数リスト　*
*    作成日／更新日　　　：　2007/05/18                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　各事業所の受領件数リストを　　　　*
*    処理概要　　　　　　：　発行する。　　　　　　　　　　　　*
*    作成日／更新日　　　：　2014/03/07                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＤＣＭカーマ東日本出荷対応　　　　*
*    作成日／更新日　　　：　2018/02/20                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ケーヨー追加　　　　　　　　　　　*
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJH8732L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/18.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<ホーマック資材　北海道事業部>>----*
     SELECT   HCSZHOK   ASSIGN         DA-01-S-HCSZHOK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HZH-ST.
*----<<ホーマック資材　東北事業部>>----*
     SELECT   HCSZTOH   ASSIGN         DA-01-S-HCSZTOH
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HZT-ST.
*----<<ホーマック資材　関東事業部>>----*
     SELECT   HCSZKNT   ASSIGN         DA-01-S-HCSZKNT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HZK-ST.
*----<<カーマ資材>>----*
     SELECT   KAHMASZ   ASSIGN         DA-01-S-KAHMASZ
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KMZ-ST.
*----<<カーマ植物>>----*
     SELECT   KAHMASK   ASSIGN         DA-01-S-KAHMASK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KMK-ST.
*----<<カーマ資材２>>----*
     SELECT   KAHMASZ2  ASSIGN         DA-01-S-KAHMASZ2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KMZ2-ST.
*----<<カーマ植物２>>----*
     SELECT   KAHMASK2  ASSIGN         DA-01-S-KAHMASK2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KMK2-ST.
*#2018/02/20 NAV ST ***************************
*----<<ケーヨー資材東>>----*
     SELECT   DCMKEIY1  ASSIGN         DA-01-S-DCMKEIY1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KEI1-ST.
*----<<ケーヨー資材西>>----*
     SELECT   DCMKEIY2  ASSIGN         DA-01-S-DCMKEIY2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KEI2-ST.
*----<<ケーヨー植物東>>----*
     SELECT   DCMKEIY3  ASSIGN         DA-01-S-DCMKEIY3
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KEI3-ST.
*----<<ケーヨー植物西>>----*
     SELECT   DCMKEIY4  ASSIGN         DA-01-S-DCMKEIY4
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KEI4-ST.
*#2018/02/20 NAV ED ***************************
*----<<ホーマック植物　北海道事業部>>----*
     SELECT   HCSKHOK   ASSIGN         DA-01-S-HCSKHOK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKH-ST.
*----<<ホーマック植物　東北事業部>>----*
     SELECT   HCSKTOH   ASSIGN         DA-01-S-HCSKTOH
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKT-ST.
*----<<ホーマック植物　関東事業部>>----*
     SELECT   HCSKKNT   ASSIGN         DA-01-S-HCSKKNT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKK-ST.
*----<<ダイキ資材>>----*
     SELECT   DAIK1     ASSIGN         DA-01-S-DAIK1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI1-ST.
*----<<ダイキ植物>>----*
     SELECT   DAIK2     ASSIGN         DA-01-S-DAIK2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI2-ST.
*----<<ダイキ植物>>----*
     SELECT   DAIK3     ASSIGN         DA-01-S-DAIK3
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI3-ST.
*----<<ダイキ資材>>----*
     SELECT   DAIK4     ASSIGN         DA-01-S-DAIK4
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI4-ST.
*----<<ダイキ植物>>----*
     SELECT   DAIK5     ASSIGN         DA-01-S-DAIK5
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI5-ST.
*----<<ダイキ植物>>----*
     SELECT   DAIK6     ASSIGN         DA-01-S-DAIK6
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI6-ST.
*----<<その他事業部>>----*
     SELECT   ERRORF    ASSIGN         DA-01-S-ERRORF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ERR-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<ホーマック資材　北海道事業部>>----*
 FD  HCSZHOK
                        BLOCK CONTAINS 1 RECORDS.
 01  HZH-REC            PIC  X(128).
*----<<ホーマック資材　東北事業部>>----*
 FD  HCSZTOH
                        BLOCK CONTAINS 1 RECORDS.
 01  HZT-REC            PIC  X(128).
*----<<ホーマック資材　関東事業部>>----*
 FD  HCSZKNT
                        BLOCK CONTAINS 1 RECORDS.
 01  HZK-REC            PIC  X(128).
*----<<カーマ　資材>>----*
 FD  KAHMASZ
                        BLOCK CONTAINS 1 RECORDS.
 01  KMZ-REC            PIC  X(128).
*----<<カーマ　植物>>----*
 FD  KAHMASK
                        BLOCK CONTAINS 1 RECORDS.
 01  KMK-REC            PIC  X(128).
*----<<カーマ　資材２>>----*
 FD  KAHMASZ2
                        BLOCK CONTAINS 1 RECORDS.
 01  KMZ2-REC           PIC  X(128).
*----<<カーマ　植物２>>----*
 FD  KAHMASK2
                        BLOCK CONTAINS 1 RECORDS.
 01  KMK2-REC           PIC  X(128).
*----<<ホーマック植物　北海道事業部>>----*
 FD  HCSKHOK
                        BLOCK CONTAINS 1 RECORDS.
 01  HKH-REC            PIC  X(128).
*----<<ホーマック植物　東北事業部>>----*
 FD  HCSKTOH
                        BLOCK CONTAINS 1 RECORDS.
 01  HKT-REC            PIC  X(128).
*----<<ホーマック植物　関東事業部>>----*
 FD  HCSKKNT
                        BLOCK CONTAINS 1 RECORDS.
 01  HKK-REC            PIC  X(128).
*----<<ダイキ資材>>----*
 FD  DAIK1
                        BLOCK CONTAINS 1 RECORDS.
 01  DI1-REC            PIC  X(128).
*----<<ダイキ植物>>----*
 FD  DAIK2
                        BLOCK CONTAINS 1 RECORDS.
 01  DI2-REC            PIC  X(128).
*----<<ダイキ植物>>----*
 FD  DAIK3
                        BLOCK CONTAINS 1 RECORDS.
 01  DI3-REC            PIC  X(128).
*----<<ダイキ資材>>----*
 FD  DAIK4
                        BLOCK CONTAINS 1 RECORDS.
 01  DI4-REC            PIC  X(128).
*----<<ダイキ植物>>----*
 FD  DAIK5
                        BLOCK CONTAINS 1 RECORDS.
 01  DI5-REC            PIC  X(128).
*----<<ダイキ植物>>----*
 FD  DAIK6
                        BLOCK CONTAINS 1 RECORDS.
 01  DI6-REC            PIC  X(128).
*#2018/02/20 NAV ST ***************************
*----<<ケーヨー資材東>>----*
 FD  DCMKEIY1
                        BLOCK CONTAINS 1 RECORDS.
 01  KEI1-REC           PIC  X(128).
*----<<ケーヨー資材西>>----*
 FD  DCMKEIY2
                        BLOCK CONTAINS 1 RECORDS.
 01  KEI2-REC           PIC  X(128).
*----<<ケーヨー植物東>>----*
 FD  DCMKEIY3
                        BLOCK CONTAINS 1 RECORDS.
 01  KEI3-REC           PIC  X(128).
*----<<ケーヨー資材東>>----*
 FD  DCMKEIY4
                        BLOCK CONTAINS 1 RECORDS.
 01  KEI4-REC           PIC  X(128).
*#2018/02/20 NAV ED ***************************
*----<<その他事業部>>----*
 FD  ERRORF
                        BLOCK CONTAINS 1 RECORDS.
 01  ERR-REC            PIC  X(128).
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  HZH-FLG        PIC  X(03)   VALUE SPACE.
     03  HZT-FLG        PIC  X(03)   VALUE SPACE.
     03  HZK-FLG        PIC  X(03)   VALUE SPACE.
     03  KMZ-FLG        PIC  X(03)   VALUE SPACE.
     03  KMK-FLG        PIC  X(03)   VALUE SPACE.
     03  KMZ2-FLG       PIC  X(03)   VALUE SPACE.
     03  KMK2-FLG       PIC  X(03)   VALUE SPACE.
     03  HKH-FLG        PIC  X(03)   VALUE SPACE.
     03  HKT-FLG        PIC  X(03)   VALUE SPACE.
     03  HKK-FLG        PIC  X(03)   VALUE SPACE.
     03  DI1-FLG        PIC  X(03)   VALUE SPACE.
     03  DI2-FLG        PIC  X(03)   VALUE SPACE.
     03  DI3-FLG        PIC  X(03)   VALUE SPACE.
     03  DI4-FLG        PIC  X(03)   VALUE SPACE.
     03  DI5-FLG        PIC  X(03)   VALUE SPACE.
     03  DI6-FLG        PIC  X(03)   VALUE SPACE.
*#2018/02/22 NAV ST
     03  KEI1-FLG       PIC  X(03)   VALUE SPACE.
     03  KEI2-FLG       PIC  X(03)   VALUE SPACE.
     03  KEI3-FLG       PIC  X(03)   VALUE SPACE.
     03  KEI4-FLG       PIC  X(03)   VALUE SPACE.
*#2018/02/22 NAV ED
     03  ERR-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  HZH-CNT        PIC  9(07).
     03  HZT-CNT        PIC  9(07).
     03  HZK-CNT        PIC  9(07).
     03  KMZ-CNT        PIC  9(07).
     03  KMK-CNT        PIC  9(07).
     03  KMZ2-CNT       PIC  9(07).
     03  KMK2-CNT       PIC  9(07).
     03  HKH-CNT        PIC  9(07).
     03  HKT-CNT        PIC  9(07).
     03  HKK-CNT        PIC  9(07).
     03  DI1-CNT        PIC  9(07).
     03  DI2-CNT        PIC  9(07).
     03  DI3-CNT        PIC  9(07).
     03  DI4-CNT        PIC  9(07).
     03  DI5-CNT        PIC  9(07).
     03  DI6-CNT        PIC  9(07).
*#2018/02/22 NAV ST
     03  KEI1-CNT       PIC  9(07).
     03  KEI2-CNT       PIC  9(07).
     03  KEI3-CNT       PIC  9(07).
     03  KEI4-CNT       PIC  9(07).
*#2018/02/22 NAV ED
     03  ERR-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  HZH-ST         PIC  X(02).
     03  HZT-ST         PIC  X(02).
     03  HZK-ST         PIC  X(02).
     03  KMZ-ST         PIC  X(02).
     03  KMK-ST         PIC  X(02).
     03  KMZ2-ST        PIC  X(02).
     03  KMK2-ST        PIC  X(02).
     03  HKH-ST         PIC  X(02).
     03  HKT-ST         PIC  X(02).
     03  HKK-ST         PIC  X(02).
     03  DI1-ST         PIC  X(02).
     03  DI2-ST         PIC  X(02).
     03  DI3-ST         PIC  X(02).
     03  DI4-ST         PIC  X(02).
     03  DI5-ST         PIC  X(02).
     03  DI6-ST         PIC  X(02).
*#2018/02/22 NAV ST
     03  KEI1-ST        PIC  X(02).
     03  KEI2-ST        PIC  X(02).
     03  KEI3-ST        PIC  X(02).
     03  KEI4-ST        PIC  X(02).
*#2018/02/22 NAV ED
     03  ERR-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SJH8732L".
 01  WK-MSG1           PIC  N(08)
                       VALUE NC"受領データ有り。".
 01  WK-MSG2           PIC  N(08)
                       VALUE NC"受領データ無し。".
 01  WK-MSG3           PIC  N(15)
                       VALUE NC"振分できない事業所データ有り。".
 01  WK-MSG4           PIC  N(15)
                       VALUE NC"振分できない事業所データ無し。".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  HD1-00                  PIC  X(08).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  HD1-005                 PIC  N(02)
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(23)  VALUE
         NC"※※受領ＤＣＭＪＡＰＡＮ事業部別件数リスト※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"事業部"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(22)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"受領件数"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(25)  VALUE  SPACE.
     03  DT1-01                  PIC  9(04).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-01A                 PIC  X(01).
     03  DT1-015                 PIC  9(08).
     03  DT1-01B                 PIC  X(01).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-02                  PIC  N(08).
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT1-03                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-04                  PIC  N(15).
 01  DT2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT2-01                  PIC  N(14).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 HCSZHOKERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSZHOK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L HCSZHOK ERROR " HZH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSZTOHERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSZTOH.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L HCSZTOH ERROR " HZT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSZKNTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSZKNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L HCSZKNT ERROR " HZK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KAHMASZERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KAHMASZ.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L KAHMASZ ERROR " KMZ-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KAHMASKERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KAHMASK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L KAHMASK ERROR " KMK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KAHMASZ2ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KAHMASZ2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L KAHMASZ2 ERROR " KMZ2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KAHMASK2ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KAHMASK2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L KAHMASK2 ERROR " KMK2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSKHOKERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSKHOK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L HCSKHOK ERROR " HKH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSKTOHERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSKTOH.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L HCSKTOH ERROR " HKT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSKKNTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSKKNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L HCSKKNT ERROR " HKK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DAIK1ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DAIK1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DAIK1 ERROR " DI1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DAIK2ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DAIK2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DAIK2 ERROR " DI2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DAIK3ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DAIK3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DAIK3 ERROR " DI3-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DAIK4ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DAIK4.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DAIK4 ERROR " DI4-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DAIK5ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DAIK5.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DAIK4 ERROR " DI5-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DAIK6ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DAIK6.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DAIK6 ERROR " DI6-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*#2018/02/22 NAV ST
 DCMKEIY1ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DCMKEIY1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DCMKEIY1 ERROR " KEI1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DCMKEIY2ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DCMKEIY2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DCMKEIY2 ERROR " KEI2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DCMKEIY3ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DCMKEIY3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DCMKEIY3 ERROR " KEI3-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DCMKEIY4ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DCMKEIY4.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L DCMKEIY4 ERROR " KEI4-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*#2018/02/22 NAV ED
 ERRORFERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ERRORF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L ERRORF ERROR " ERR-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8732L PRTFILE ERROR " PRT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJH8732L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HCSZHOK HCSZTOH HCSZKNT KAHMASZ.
     OPEN     INPUT     HCSKHOK HCSKTOH HCSKKNT KAHMASK.
     OPEN     INPUT     DAIK1   DAIK2   DAIK3   ERRORF.
     OPEN     INPUT     DAIK4   DAIK5   DAIK6.
     OPEN     INPUT     KAHMASZ2  KAHMASK2.
*#2018/02/22 NAV ST
     OPEN     INPUT     DCMKEIY1 DCMKEIY2 DCMKEIY3 DCMKEIY4.
*#2018/02/22 NAV ED
     OPEN     OUTPUT    PRTFILE.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*ヘッダ行印字
     PERFORM       HEAD-WT-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*ホーマック資材　北海道事業部件数カウント
*    件数カウント
     PERFORM HCSZHOK-RD-SEC  UNTIL  HZH-FLG = "END".
*    帳票エリアセット
     IF   HZH-CNT  >  ZERO
          MOVE    01             TO  DT1-01
          MOVE NC"ＨＣ資材　北海道"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    883                TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    HZH-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    01             TO  DT1-01
          MOVE NC"ＨＣ資材　北海道"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    883                TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ホーマック資材　東北事業部件数カウント
*    件数カウント
     PERFORM HCSZTOH-RD-SEC  UNTIL  HZT-FLG = "END".
*    帳票エリアセット
     IF   HZT-CNT > ZERO
          MOVE    02             TO  DT1-01
          MOVE NC"ＨＣ資材　東北　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    882                TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    HZT-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    02             TO  DT1-01
          MOVE NC"ＨＣ資材　東北　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    882                TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ホーマック資材　関東事業部件数カウント
*    件数カウント
     PERFORM HCSZKNT-RD-SEC  UNTIL  HZK-FLG = "END".
*    帳票エリアセット
     IF   HZK-CNT > ZERO
          MOVE    03             TO  DT1-01
          MOVE NC"ＨＣ資材　関東　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    880                TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    HZK-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    03             TO  DT1-01
          MOVE NC"ＨＣ資材　関東　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    880                TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    HZK-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*カーマ資材　件数カウント
*    件数カウント
     PERFORM KAHMASZ-RD-SEC  UNTIL  KMZ-FLG = "END".
*    帳票エリアセット
     IF   KMZ-CNT > ZERO
          MOVE    04             TO  DT1-01
          MOVE NC"カーマ　資材　　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    13938              TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KMZ-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    04             TO  DT1-01
          MOVE NC"カーマ　資材　　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    13938              TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KMZ-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*カーマ植物　件数カウント
*    件数カウント
     PERFORM KAHMASK-RD-SEC  UNTIL  KMK-FLG = "END".
*    帳票エリアセット
     IF   KMK-CNT > ZERO
          MOVE    05             TO  DT1-01
          MOVE NC"カーマ　植物　　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    17137              TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KMK-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    05             TO  DT1-01
          MOVE NC"カーマ　植物　　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    17137              TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KMK-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*カーマ資材２件数カウント
*    件数カウント
     PERFORM KAHMASZ2-RD-SEC  UNTIL  KMZ2-FLG = "END".
*    帳票エリアセット
     IF   KMZ2-CNT > ZERO
          MOVE    06             TO  DT1-01
          MOVE NC"カーマ　資材２　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    139381             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KMZ2-CNT       TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    06             TO  DT1-01
          MOVE NC"カーマ　資材２　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    139381             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KMZ2-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*カーマ植物２件数カウント
*    件数カウント
     PERFORM KAHMASK2-RD-SEC  UNTIL  KMK2-FLG = "END".
*    帳票エリアセット
     IF   KMK2-CNT > ZERO
          MOVE    07             TO  DT1-01
          MOVE NC"カーマ　植物２　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    171371             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KMK2-CNT       TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    07             TO  DT1-01
          MOVE NC"カーマ　植物２　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    171371             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KMK2-CNT       TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ホーマック植物　北海道事業部件数カウント
*    件数カウント
     PERFORM HCSKHOK-RD-SEC  UNTIL  HKH-FLG = "END".
*    帳票エリアセット
     IF   HKH-CNT  >  ZERO
          MOVE    08             TO  DT1-01
          MOVE NC"ＨＣ植物　北海道"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    14273              TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    HKH-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    08             TO  DT1-01
          MOVE NC"ＨＣ植物　北海道"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    14273              TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ホーマック植物　東北事業部件数カウント
*    件数カウント
     PERFORM HCSKTOH-RD-SEC  UNTIL  HKT-FLG = "END".
*    帳票エリアセット
     IF   HKT-CNT > ZERO
          MOVE    09             TO  DT1-01
          MOVE NC"ＨＣ植物　東北　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    14272              TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    HKT-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    09             TO  DT1-01
          MOVE NC"ＨＣ植物　東北　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    14272              TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ホーマック植物　関東事業部件数カウント
*    件数カウント
     PERFORM HCSKKNT-RD-SEC  UNTIL  HKK-FLG = "END".
*    帳票エリアセット
     IF   HKK-CNT > ZERO
          MOVE    10             TO  DT1-01
          MOVE NC"ＨＣ植物　関東　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    1427               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    HKK-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    10             TO  DT1-01
          MOVE NC"ＨＣ植物　関東　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    1427               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ダイキ資材　件数カウント
*    件数カウント
     PERFORM DAIK1-RD-SEC  UNTIL  DI1-FLG = "END".
*    帳票エリアセット
     IF   DI1-CNT > ZERO
          MOVE    11             TO  DT1-01
          MOVE NC"ダイキ資材４０３"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100403             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI1-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    11             TO  DT1-01
          MOVE NC"ダイキ資材４０３"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100403             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI1-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ダイキ植物　件数カウント
*    件数カウント
     PERFORM DAIK2-RD-SEC  UNTIL  DI2-FLG = "END".
*    帳票エリアセット
     IF   DI1-CNT > ZERO
          MOVE    12             TO  DT1-01
          MOVE NC"ダイキ資材４４１"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100441             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI2-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    12             TO  DT1-01
          MOVE NC"ダイキ資材４４１"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100441             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI2-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ダイキ植物　件数カウント
*    件数カウント
     PERFORM DAIK3-RD-SEC  UNTIL  DI3-FLG = "END".
*    帳票エリアセット
     IF   DI3-CNT > ZERO
          MOVE    13             TO  DT1-01
          MOVE NC"ダイキ資材４２７"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100427             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI3-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    13             TO  DT1-01
          MOVE NC"ダイキ資材４２７"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100427             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI3-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ダイキ資材　件数カウント
*    件数カウント
     PERFORM DAIK4-RD-SEC  UNTIL  DI4-FLG = "END".
*    帳票エリアセット
     IF   DI4-CNT > ZERO
          MOVE    14             TO  DT1-01
          MOVE NC"サンコー資４０４"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100404             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI4-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    14             TO  DT1-01
          MOVE NC"サンコー資４０４"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100403             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI4-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ダイキ植物　件数カウント
*    件数カウント
     PERFORM DAIK5-RD-SEC  UNTIL  DI5-FLG = "END".
*    帳票エリアセット
     IF   DI5-CNT > ZERO
          MOVE    15             TO  DT1-01
          MOVE NC"サンコー資４４２"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100442             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI5-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    15             TO  DT1-01
          MOVE NC"ダイキ資材４４１"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100441             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI5-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*ダイキ植物　件数カウント
*    件数カウント
     PERFORM DAIK6-RD-SEC  UNTIL  DI6-FLG = "END".
*    帳票エリアセット
     IF   DI6-CNT > ZERO
          MOVE    16             TO  DT1-01
          MOVE NC"サンコー植４２８"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100428             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI6-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    16             TO  DT1-01
          MOVE NC"サンコー植４２８"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    100428             TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    DI6-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*#2018/02/22 NAV ST
*    件数カウント
     PERFORM KEIYO1-RD-SEC  UNTIL  KEI1-FLG = "END".
*    帳票エリアセット
     IF   KEI1-CNT > ZERO
          MOVE    17                 TO  DT1-01
          MOVE NC"ケーヨー資材　東"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    1731               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KEI1-CNT           TO  DT1-03
          MOVE    WK-MSG1            TO  DT1-04
          WRITE   PRT-REC   FROM   DT1   AFTER  1
          WRITE   PRT-REC   FROM   SEN1  AFTER  1
          MOVE    "CHK"              TO  CHK-FLG
     ELSE
          MOVE    17                 TO  DT1-01
          MOVE NC"ケーヨー資材　東"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    1731               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KEI1-CNT           TO  DT1-03
          MOVE    WK-MSG2            TO  DT1-04
          WRITE   PRT-REC   FROM   DT1   AFTER  1
          WRITE   PRT-REC   FROM   SEN1  AFTER  1
          MOVE    "CHK"              TO  CHK-FLG
     END-IF.
*    件数カウント
     PERFORM KEIYO2-RD-SEC  UNTIL  KEI2-FLG = "END".
*    帳票エリアセット
     IF   KEI2-CNT > ZERO
          MOVE    18                 TO  DT1-01
          MOVE NC"ケーヨー資材　西"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    1732               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KEI2-CNT           TO  DT1-03
          MOVE    WK-MSG1            TO  DT1-04
          WRITE   PRT-REC   FROM   DT1   AFTER  1
          WRITE   PRT-REC   FROM   SEN1  AFTER  1
          MOVE    "CHK"              TO  CHK-FLG
     ELSE
          MOVE    18                 TO  DT1-01
          MOVE NC"ケーヨー資材　西"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    1732               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KEI2-CNT           TO  DT1-03
          MOVE    WK-MSG2            TO  DT1-04
          WRITE   PRT-REC   FROM   DT1   AFTER  1
          WRITE   PRT-REC   FROM   SEN1  AFTER  1
          MOVE    "CHK"              TO  CHK-FLG
     END-IF.
*    件数カウント
     PERFORM KEIYO3-RD-SEC  UNTIL  KEI3-FLG = "END".
*    帳票エリアセット
     IF   KEI3-CNT > ZERO
          MOVE    19                 TO  DT1-01
          MOVE NC"ケーヨー植物　東"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    7601               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KEI3-CNT           TO  DT1-03
          MOVE    WK-MSG1            TO  DT1-04
          WRITE   PRT-REC   FROM   DT1   AFTER  1
          WRITE   PRT-REC   FROM   SEN1  AFTER  1
          MOVE    "CHK"              TO  CHK-FLG
     ELSE
          MOVE    19                 TO  DT1-01
          MOVE NC"ケーヨー植物　東"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    7601               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KEI3-CNT           TO  DT1-03
          MOVE    WK-MSG2            TO  DT1-04
          WRITE   PRT-REC   FROM   DT1   AFTER  1
          WRITE   PRT-REC   FROM   SEN1  AFTER  1
          MOVE    "CHK"              TO  CHK-FLG
     END-IF.
*    件数カウント
     PERFORM KEIYO4-RD-SEC  UNTIL  KEI4-FLG = "END".
*    帳票エリアセット
     IF   KEI4-CNT > ZERO
          MOVE    20                 TO  DT1-01
          MOVE NC"ケーヨー資材　東"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    7602               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KEI4-CNT           TO  DT1-03
          MOVE    WK-MSG1            TO  DT1-04
          WRITE   PRT-REC   FROM   DT1   AFTER  1
          WRITE   PRT-REC   FROM   SEN1  AFTER  1
          MOVE    "CHK"              TO  CHK-FLG
     ELSE
          MOVE    20                 TO  DT1-01
          MOVE NC"ケーヨー資材　東"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    7602               TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    KEI4-CNT           TO  DT1-03
          MOVE    WK-MSG2            TO  DT1-04
          WRITE   PRT-REC   FROM   DT1   AFTER  1
          WRITE   PRT-REC   FROM   SEN1  AFTER  1
          MOVE    "CHK"              TO  CHK-FLG
     END-IF.
*#2018/02/22 NAV ED
*振り分けエラー件数カウント
*    件数カウント
     PERFORM ERRORF-RD-SEC  UNTIL  ERR-FLG = "END".
*    帳票エリアセット
     IF   ERR-CNT > ZERO
          MOVE    99             TO  DT1-01
          MOVE NC"振分エラー　　　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    00000000           TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    ERR-CNT        TO  DT1-03
          MOVE    WK-MSG3        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    99             TO  DT1-01
          MOVE NC"振分エラー　　　"  TO  DT1-02
          MOVE "("                   TO  DT1-01A
          MOVE    00000000           TO  DT1-015
          MOVE ")"                   TO  DT1-01B
          MOVE    ERR-CNT        TO  DT1-03
          MOVE    WK-MSG4        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*    件数チェック
     IF   CHK-FLG = SPACE
          MOVE    WK-MSG2        TO  DT2-01
          WRITE   PRT-REC   FROM   DT2    AFTER  5
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HCSZHOK HCSZTOH HCSZKNT KAHMASZ.
     CLOSE    HCSKHOK HCSKTOH HCSKKNT KAHMASK.
     CLOSE    DAIK1   DAIK2   DAIK3   ERRORF  PRTFILE.
     CLOSE    DAIK4   DAIK5   DAIK6.
     CLOSE    KAHMASZ2  KAHMASK2.
*#2018/02/22 NAV ST
     CLOSE    DCMKEIY3  DCMKEIY4  DCMKEIY1  DCMKEIY2.
*#2018/02/22 NAV ED
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJH8732L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック資材　北海道データ読込み                 *
*--------------------------------------------------------------*
 HCSZHOK-RD-SEC             SECTION.
     READ   HCSZHOK   AT  END
            MOVE  "END"  TO  HZH-FLG
            NOT   AT  END
            ADD    1     TO  HZH-CNT
     END-READ.
 HCSZHOK-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック資材　東北　データ読込み                 *
*--------------------------------------------------------------*
 HCSZTOH-RD-SEC             SECTION.
     READ   HCSZTOH   AT  END
            MOVE  "END"  TO  HZT-FLG
            NOT   AT  END
            ADD    1     TO  HZT-CNT
     END-READ.
 HCSZTOH-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック資材　関東　データ読込み                 *
*--------------------------------------------------------------*
 HCSZKNT-RD-SEC             SECTION.
     READ   HCSZKNT   AT  END
            MOVE  "END"  TO  HZK-FLG
            NOT   AT  END
            ADD    1     TO  HZK-CNT
     END-READ.
 HCSZKNT-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           カーマ資材　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 KAHMASZ-RD-SEC             SECTION.
     READ   KAHMASZ   AT  END
            MOVE  "END"  TO  KMZ-FLG
            NOT   AT  END
            ADD    1     TO  KMZ-CNT
     END-READ.
 KAHMASZ-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           カーマ植物　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 KAHMASK-RD-SEC             SECTION.
     READ   KAHMASK   AT  END
            MOVE  "END"  TO  KMK-FLG
            NOT   AT  END
            ADD    1     TO  KMK-CNT
     END-READ.
 KAHMASK-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           カーマ資材２　　　　　データ読込み                 *
*--------------------------------------------------------------*
 KAHMASZ2-RD-SEC             SECTION.
     READ   KAHMASZ2  AT  END
            MOVE  "END"  TO  KMZ2-FLG
            NOT   AT  END
            ADD    1     TO  KMZ2-CNT
     END-READ.
 KAHMASZ2-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           カーマ植物２　　　　　データ読込み                 *
*--------------------------------------------------------------*
 KAHMASK2-RD-SEC             SECTION.
     READ   KAHMASK2  AT  END
            MOVE  "END"  TO  KMK2-FLG
            NOT   AT  END
            ADD    1     TO  KMK2-CNT
     END-READ.
 KAHMASK2-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック植物　北海道データ読込み                 *
*--------------------------------------------------------------*
 HCSKHOK-RD-SEC             SECTION.
     READ   HCSKHOK   AT  END
            MOVE  "END"  TO  HKH-FLG
            NOT   AT  END
            ADD    1     TO  HKH-CNT
     END-READ.
 HCSKHOK-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック植物　東北　データ読込み                 *
*--------------------------------------------------------------*
 HCSKTOH-RD-SEC             SECTION.
     READ   HCSKTOH   AT  END
            MOVE  "END"  TO  HKT-FLG
            NOT   AT  END
            ADD    1     TO  HKT-CNT
     END-READ.
 HCSKTOH-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック植物　関東　データ読込み                 *
*--------------------------------------------------------------*
 HCSKKNT-RD-SEC             SECTION.
     READ   HCSKKNT   AT  END
            MOVE  "END"  TO  HKK-FLG
            NOT   AT  END
            ADD    1     TO  HKK-CNT
     END-READ.
 HCSKKNT-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK1-RD-SEC             SECTION.
     READ   DAIK1   AT  END
            MOVE  "END"  TO  DI1-FLG
            NOT   AT  END
            ADD    1     TO  DI1-CNT
     END-READ.
 DAIK1-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK2-RD-SEC             SECTION.
     READ   DAIK2   AT  END
            MOVE  "END"  TO  DI2-FLG
            NOT   AT  END
            ADD    1     TO  DI2-CNT
     END-READ.
 DAIK2-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK3-RD-SEC             SECTION.
     READ   DAIK3   AT  END
            MOVE  "END"  TO  DI3-FLG
            NOT   AT  END
            ADD    1     TO  DI3-CNT
     END-READ.
 DAIK3-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK4-RD-SEC             SECTION.
     READ   DAIK4   AT  END
            MOVE  "END"  TO  DI4-FLG
            NOT   AT  END
            ADD    1     TO  DI4-CNT
     END-READ.
 DAIK4-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK5-RD-SEC             SECTION.
     READ   DAIK5   AT  END
            MOVE  "END"  TO  DI5-FLG
            NOT   AT  END
            ADD    1     TO  DI5-CNT
     END-READ.
 DAIK5-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK6-RD-SEC             SECTION.
     READ   DAIK6   AT  END
            MOVE  "END"  TO  DI6-FLG
            NOT   AT  END
            ADD    1     TO  DI6-CNT
     END-READ.
 DAIK6-RD-EXIT.
     EXIT.
*#2018/02/22 NAV ST
*--------------------------------------------------------------*
*           ケーヨー資材　東日本　データ読込み                 *
*--------------------------------------------------------------*
 KEIYO1-RD-SEC            SECTION.
     READ   DCMKEIY1  AT  END
            MOVE  "END"  TO  KEI1-FLG
            NOT   AT  END
            ADD    1     TO  KEI1-CNT
     END-READ.
 KEIYO1-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ケーヨー資材　東日本　データ読込み                 *
*--------------------------------------------------------------*
 KEIYO2-RD-SEC            SECTION.
     READ   DCMKEIY2  AT  END
            MOVE  "END"  TO  KEI2-FLG
            NOT   AT  END
            ADD    1     TO  KEI2-CNT
     END-READ.
 KEIYO2-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ケーヨー資材　東日本　データ読込み                 *
*--------------------------------------------------------------*
 KEIYO3-RD-SEC            SECTION.
     READ   DCMKEIY3  AT  END
            MOVE  "END"  TO  KEI3-FLG
            NOT   AT  END
            ADD    1     TO  KEI3-CNT
     END-READ.
 KEIYO3-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ケーヨー資材　東日本　データ読込み                 *
*--------------------------------------------------------------*
 KEIYO4-RD-SEC            SECTION.
     READ   DCMKEIY4  AT  END
            MOVE  "END"  TO  KEI4-FLG
            NOT   AT  END
            ADD    1     TO  KEI4-CNT
     END-READ.
 KEIYO4-RD-EXIT.
     EXIT.
*#2018/02/22 NAV ED
*--------------------------------------------------------------*
*                  エラー　データ読込み                        *
*--------------------------------------------------------------*
 ERRORF-RD-SEC             SECTION.
     READ   ERRORF   AT  END
            MOVE  "END"  TO  ERR-FLG
            NOT   AT  END
            ADD    1     TO  ERR-CNT
     END-READ.
 ERRORF-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HD1-01.
     MOVE     LINK-OUT-YMD(5:2)   TO        HD1-02.
     MOVE     LINK-OUT-YMD(7:2)   TO        HD1-03.
***  ページ■
     MOVE     1                   TO        HD1-04.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   SEN       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
 HEAD-WT-EXIT.
     EXIT.
