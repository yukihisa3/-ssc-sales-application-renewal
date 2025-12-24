****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　　*
*    サブシステム　　　　：　コーナン　ＥＤＩ　　　　　　　　　*
*    モジュール名　　　　：　コーナン発注データ変換　　　　　　*
*    　　　　　　　　　　　　（ＰＯＲ一括）　　　　　　　　　　*
*    作成日／作成者　　　：　2021/01/08 INOUE                  *
*    処理概要　　　　　　：　コーナンＰＯＲ／ケースデータから　*
*                            変換伝票データファイルを作成する　*
*                            コーナン商事用                    *
*    変更日／変更者　　　：　2023/08/28 NAV TAKAHASHI          *
*    変更概要　　　　　　：　得意先ＣＤ枝番対応　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH1218B.
*                  流用:NJH1202B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/01/08.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＰＯＲ／ケースデータ
     SELECT   KONANPOR  ASSIGN    TO        DA-01-S-KONANPOR
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*変換伝票データ
     SELECT   JHSHENL1  ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY                 HEN-F46
                                            HEN-F47   HEN-F01
                                            HEN-F02   HEN-F03
                        FILE  STATUS   IS   HEN-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*店舗マスタ索引
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE STATUS    IS   TEN-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*出荷場所件数マスタ
     SELECT   JSMKENL1  ASSIGN    TO        DA-01-VI-JSMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*当日スケジュールマスタ
     SELECT   JSMDAYL1  ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
*コーナン基本情報ファイル
     SELECT   KNJOHOF   ASSIGN    TO        DA-01-VI-KNJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F001  JOH-F002
                                            JOH-F003  JOH-F004
                                            JOH-FA02  JOH-FA07
                                            JOH-FA08  JOH-FA03
                                            JOH-FA04  JOH-FA05
                        FILE  STATUS   IS   JOH-STATUS.
*コーナン受領データファイル
*    SELECT   KNJYRF    ASSIGN    TO        DA-01-VS-KNJYRF
*                       ACCESS    MODE IS   SEQUENTIAL
*                       FILE      STATUS    KNJ-STATUS
*                       ORGANIZATION   IS   SEQUENTIAL.
*
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　ＢＦ＝　１
******************************************************************
 FD  KONANPOR
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
*    03  DEN-01                  OCCURS 2.
     03  DEN-01.
         05  DEN-01A             PIC  X(02).
         05  DEN-01B             PIC  X(126).
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
******************************************************************
*    出荷場所別件数ファイル
******************************************************************
 FD  JSMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     KNSHIRED  OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
******************************************************************
*    ＶＬＤ５００
******************************************************************
 FD  VLD500.
 01  VLD-REC.
     03  VLD-F01           PIC  X(02).
     03  VLD-F02           PIC  9(03).
     03  VLD-F03           PIC  X(02).
     03  VLD-F04           PIC  X(08).
     03  VLD-F05           PIC  9(06).
     03  VLD-F06           PIC  9(01).
     03  VLD-F07           PIC  X(02).
     03  VLD-F08           PIC  9(02).
     03  VLD-F09           PIC  9(02).
     03  VLD-F10           PIC  9(04).
     03  VLD-F11           PIC  9(08).
     03  VLD-F12           PIC  9(04).
     03  VLD-F13           PIC  9(08).
     03  FILLER            PIC  X(48).
*
******************************************************************
*    コーナン基本情報ファイル
******************************************************************
 FD  KNJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     KNJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
*
******************************************************************
*    コーナン受領データファイル
******************************************************************
*FD  KNJYRF
*                       LABEL RECORD   IS   STANDARD.
*    COPY     KNJYRF    OF        XFDLIB
*             JOINING   KNJ   AS  PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN   AS  PREFIX.
******************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WRT-JYR-CNT             PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU1              PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D1            PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU1              PIC  9(08)     VALUE  ZERO.
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TEN-F73              PIC  X(02)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  WK-TOKCD                PIC  9(08)     VALUE  ZERO.
 01  WK-TOKCD-BK             PIC  9(08)     VALUE  ZERO.
 01  WK-HEN-F48              PIC  X(02)     VALUE  SPACE.
*
*データ情報退避ワーク
 01  WK-FLHD-REC.
     03  WK-FLHD01                PIC       X(02).
     03  WK-FLHD02                PIC       9(02).
     03  WK-FLHD03                PIC       9(06).
     03  WK-FLHD04                PIC       9(06).
     03  WK-FLHD05                PIC       9(08).
     03  WK-FLHD06                PIC       9(06).
     03  WK-FLHD07                PIC       9(06).
     03  WK-FLHD08                PIC       X(20).
     03  WK-FLHD09                PIC       X(20).
     03  WK-FLHD10                PIC       X(20).
     03  WK-FLHD11                PIC       X(08).
     03  WK-FLHD12                PIC       9(01).
     03  WK-FLHD13                PIC       X(08).
     03  WK-FLHD14                PIC       9(08).
     03  WK-FLHD15                PIC       9(01).
     03  WK-FLHD16                PIC       9(06).
*
*    伝票ヘッド退避ワーク
 01  WK-DNHD-REC.
     03  WK-DNHD01                PIC       X(02).
     03  WK-DNHD02                PIC       9(04).
     03  WK-DNHD03                PIC       9(02).
     03  WK-DNHD04                PIC       9(06).
     03  WK-DNHD05                PIC       9(06).
     03  WK-DNHD06                PIC       9(08).
     03  WK-DNHD77.
       05  WK-DNHD07              PIC       9(06).
       05  WK-DNHD08              PIC       9(02).
     03  WK-DNHD09                PIC       9(02).
     03  WK-DNHD10                PIC       9(04).
     03  WK-DNHD11                PIC       9(02).
     03  WK-DNHD12                PIC       9(01).
     03  WK-DNHD13                PIC       9(01).
     03  WK-DNHD14                PIC       X(12).
     03  WK-DNHD15                PIC       X(40).
     03  WK-DNHD16                PIC       9(02).
     03  WK-DNHD17                PIC       X(04).
     03  WK-DNHD18                PIC       9(01).
     03  WK-DNHD19                PIC       X(08).
     03  WK-DNHD20                PIC       9(08).
     03  WK-DNHD21                PIC       9(01).
     03  WK-DNHD22                PIC       9(06).
*
*    明細レコード退避ワーク
 01  WK-DNDT-REC.
     03  WK-DNDT01                PIC       X(02).
     03  WK-DNDT02                PIC       X(20).
     03  WK-DNDT03                PIC       X(20).
     03  WK-DNDT04                PIC       9(08).
     03  WK-DNDT05                PIC       X(13).
     03  WK-DNDT06                PIC       9(03).
     03  WK-DNDT07                PIC       9(03).
     03  WK-DNDT08                PIC       9(05).
     03  WK-DNDT09                PIC       9(07)V99.
*    03  FILLER                   PIC       X(02).
     03  WK-DNDT10                PIC       9(07).
     03  WK-DNDT11                PIC       X(13).
     03  WK-DNDT12                PIC       9(01).
     03  WK-DNDT13                PIC       9(01).
*    03  FILLER                   PIC       X(16).
     03  WK-DNDT14                PIC       X(08).
     03  WK-DNDT15                PIC       9(08).
     03  WK-DNDT16                PIC       9(01).
     03  WK-DNDT17                PIC       9(06).
*
*データ情報退避ワーク（受領返品）
*01  WK-JRHD-REC.
*    03  WK-JRHD01                PIC       X(02).
*    03  WK-JRHD02                PIC       9(02).
*    03  WK-JRHD03                PIC       9(06).
*    03  WK-JRHD04                PIC       9(06).
*    03  WK-JRHD05                PIC       9(08).
*    03  WK-JRHD06                PIC       9(06).
*    03  WK-JRHD07                PIC       9(06).
*    03  WK-JRHD08                PIC       X(20).
*    03  WK-JRHD09                PIC       X(20).
*    03  WK-JRHD10                PIC       X(20).
*    03  WK-JRHD11                PIC       9(01).
*    03  WK-JRHD12                PIC       X(24).
*    03  WK-JRHD13                PIC       9(01).
*    03  WK-JRHD14                PIC       9(06).
*
*    伝票ヘッド＆明細退避ワーク
*01  WK-JRDT-REC.
*    03  WK-JRDT01                PIC       X(02).
*    03  WK-JRDT02                PIC       9(06).
*    03  WK-JRDT03                PIC       9(03).
*    03  WK-JRDT04                PIC       9(02).
*    03  WK-JRDT05                PIC       9(08).
*    03  WK-JRDT06                PIC       9(08).
*    03  WK-JRDT07                PIC       9(08).
*    03  WK-JRDT08                PIC       X(13).
*    03  WK-JRDT09                PIC       X(20).
*    03  WK-JRDT10                PIC       X(20).
*    03  WK-JRDT11                PIC       X(01).
*    03  WK-JRDT12                PIC       9(05).
*    03  WK-JRDT13                PIC       9(07).
*    03  WK-JRDT14                PIC       9(01).
*    03  WK-JRDT15                PIC       X(17).
*    03  WK-JRDT16                PIC       9(01).
*    03  WK-JRDT17                PIC       9(06).
*
 01  WK-AREA.
     03  WK-GYO            PIC 9(02).
     03  WK-DENNO          PIC 9(08).
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*システム時刻
     03  SYS-TIME          PIC 9(08).
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  KNJ-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH1218B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH1218B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH1218B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-NIS-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " NISHI = ".
         05  OUT-NIS-CNT    PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-HIG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " HIGASI= ".
         05  OUT-HIG-CNT    PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*    03  MSG-JYR-OUT.
*        05  FILLER         PIC   X(05)  VALUE " *** ".
*        05  FILLER         PIC   X(09)  VALUE " JURYOU= ".
*        05  OUT-JYR-CNT    PIC   9(06).
*        05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KONANPOR.
     MOVE      "KONANPOR"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSHENL1.
     MOVE      "JHSHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1"     TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMKENL1.
     MOVE      "JSMKENL1"   TO   AB-FILE.
     MOVE      KEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMDAYL1.
     MOVE      "JSMDAYL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   VLD500.
     MOVE      "VLD500  "   TO   AB-FILE.
     MOVE      VLD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNJOHOF.
     MOVE      "KNJOHOL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*FILEERR-SEC11          SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   KNJYRF.
*    MOVE      "KNJYRF  "   TO   AB-FILE.
*    MOVE      KNJ-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     INITIALIZE                   WK-AREA.
     OPEN     INPUT     KONANPOR
                        SHOTBL1   MEIMS1
                        JHMRUTL1  TOKMS2.
     OPEN     EXTEND    JHSHENL1  KNJOHOF.
     OPEN     I-O       JSMKENL1  JSMDAYL1.
     OPEN     OUTPUT    VLD500.
*    OPEN     OUTPUT    KNJYRF.
     OPEN     INPUT     TENMS1.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    MOVE     ZERO      TO        WRT-JYR-CNT  OUT-JYR-CNT.
*
     MOVE     SPACE     TO        WK-FLHD-REC.
     INITIALIZE                   WK-FLHD-REC.
     MOVE     SPACE     TO        WK-DNHD-REC.
     INITIALIZE                   WK-DNHD-REC.
     MOVE     SPACE     TO        WK-DNDT-REC.
     INITIALIZE                   WK-DNDT-REC.
*    MOVE     SPACE     TO        WK-JRHD-REC.
*    INITIALIZE                   WK-JRHD-REC.
*    MOVE     SPACE     TO        WK-JRDT-REC.
*    INITIALIZE                   WK-JRDT-REC.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     READ     KONANPOR
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
     ACCEPT      SYS-TIME  FROM      TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*    PERFORM  VARYING   IDX      FROM      1  BY  1
*             UNTIL     IDX      >         2
*
*ファイルヘッダ処理
     IF  ( DEN-01A       =     "FL" )  AND
         ( DEN-01B     (1:2)  = 00  )
           MOVE   SPACE        TO   WK-FLHD-REC
           INITIALIZE               WK-FLHD-REC
           MOVE   DEN-01       TO   WK-FLHD-REC
     END-IF.
*    IF  ( DEN-01A       =     "FL"        )  AND
*        ( DEN-01B     (1:2)  = 01  OR  02 )
*          MOVE   SPACE        TO   WK-JRHD-REC
*          INITIALIZE               WK-JRHD-REC
*          MOVE   DEN-01       TO   WK-JRHD-REC
*    END-IF.
*
*伝票ヘッダ処理
     IF    DEN-01A       =     "PH"
*          出荷場所件数マスタ出力
***********IF   CNT-MAISU     >    ZERO
*               PERFORM  JSMKENL1-WRT-SEC
***********END-IF
           MOVE      SPACE         TO    WK-DNHD-REC
           INITIALIZE                    WK-DNHD-REC
           MOVE      DEN-01        TO    WK-DNHD-REC
****       ADD       1        TO   CNT-MAISU
****       MOVE      ZERO     TO   CNT-KENSU-D
           MOVE      ZERO     TO   CNT-GYO
     END-IF.
*明細行
     IF    DEN-01A       =     "PD"
           MOVE      SPACE         TO    WK-DNDT-REC
           INITIALIZE                    WK-DNDT-REC
           MOVE      DEN-01        TO    WK-DNDT-REC
****       ADD       1        TO   CNT-KENSU
****       ADD       1        TO   CNT-KENSU-D
     END-IF.
*
     IF    DEN-01A       =     "PD"
           PERFORM   EDIT-SEC
     END-IF.
*
*受領データ伝票＆明細行
*       IF    DEN-01A(IDX)  =     "DJ"
*             MOVE      SPACE         TO    WK-JRDT-REC
*             INITIALIZE                    WK-JRDT-REC
*             MOVE      DEN-01(IDX)   TO    WK-JRDT-REC
*             PERFORM   EDIT-JYR-SEC
*       END-IF
*
*    END-PERFORM.
*
     READ     KONANPOR
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
     MOVE     SPACE         TO        HEN-REC  JOH-REC.
     INITIALIZE                       HEN-REC  JOH-REC.
*各量販店領域転送
*　ファイルヘッダ
*    レコード区分
     MOVE     WK-FLHD01     TO        HEN-A01.
*    ファイル種別
     MOVE     WK-FLHD02     TO        HEN-A02.
*    データ作成日
     MOVE     WK-FLHD03     TO        HEN-A03.
*    発注企業コード
     MOVE     WK-FLHD04     TO        HEN-A04.
*    受注企業コード
     MOVE     WK-FLHD05     TO        HEN-A05.
*    レコード件数
     MOVE     WK-FLHD06     TO        HEN-A06.
*    帳票枚数
     MOVE     WK-FLHD07     TO        HEN-A07.
*    発注企業名
     MOVE     WK-FLHD08     TO        HEN-A08.
*    受注企業名１
     MOVE     WK-FLHD09     TO        HEN-A09.
*    受注企業名２
     MOVE     WK-FLHD10     TO        HEN-A10.
*    送信グループコード
     MOVE     WK-FLHD12     TO        HEN-A11.
*    ＶＡＮセンターコード
     MOVE     WK-FLHD13     TO        HEN-A12.
*    配信先コード
     MOVE     WK-FLHD14     TO        HEN-A13.
*    ヘッダフラグ
     MOVE     WK-FLHD15     TO        HEN-A14.
*    ＳＥＱNO.
     MOVE     WK-FLHD16     TO        HEN-A15.
*　伝票ヘッダ
*    レコード区分
     MOVE     WK-DNHD01     TO        HEN-A16.
*    店コード
     MOVE     WK-DNHD02     TO        HEN-A17.
*    部門コード
     MOVE     WK-DNHD03     TO        HEN-A18.
*    発注日
     MOVE     WK-DNHD04     TO        HEN-A19.
*    納品指定日
     MOVE     WK-DNHD05     TO        HEN-A20.
*    伝票NO.
     MOVE     WK-DNHD06     TO        HEN-A21.
*    受注企業コード
     MOVE     WK-DNHD77     TO        HEN-A22.
*    島番号
     MOVE     WK-DNHD09     TO        HEN-A23.
*    台番
     MOVE     WK-DNHD10     TO        HEN-A24.
*    支払条件
     MOVE     WK-DNHD12     TO        HEN-A25.
*    分割回数
     MOVE     WK-DNHD13     TO        HEN-A26.
*    店名
     MOVE     WK-DNHD14     TO        HEN-A27.
*    取引先名
     MOVE     WK-DNHD15     TO        HEN-A28.
*    伝票区分
     MOVE     WK-DNHD16     TO        HEN-A29.
*    送信グループコード
     MOVE     WK-DNHD18     TO        HEN-A30.
*    ＶＡＮセンターコード
     MOVE     WK-DNHD19     TO        HEN-A31.
*    取引先コード
     MOVE     WK-DNHD20     TO        HEN-A32.
*    ヘッダフラグ
     MOVE     WK-DNHD21     TO        HEN-A33.
*    ＳＥＱNO.
     MOVE     WK-DNHD22     TO        HEN-A34.
*　伝票明細
*    レコード区分
     MOVE     WK-DNDT01     TO        HEN-A35.
*    品名
     MOVE     WK-DNDT02     TO        HEN-A36.
*    規格
     MOVE     WK-DNDT03     TO        HEN-A37.
*    商品コード
     MOVE     WK-DNDT04     TO        HEN-A38.
*    取引先商品コード
     MOVE     WK-DNDT05     TO        HEN-A39.
*    入数
     MOVE     WK-DNDT06     TO        HEN-A40.
*    ケース
     MOVE     WK-DNDT07     TO        HEN-A41.
*    数量
     MOVE     WK-DNDT08     TO        HEN-A42.
*    原価単価
     MOVE     WK-DNDT09     TO        HEN-A43.
*    売価単価
     MOVE     WK-DNDT10     TO        HEN-A44.
*    ＪＡＮコード
     MOVE     WK-DNDT11     TO        HEN-A45.
*    ケースフラグ
     MOVE     WK-DNDT12     TO        HEN-A46.
*    送信グループコード
     MOVE     WK-DNDT13     TO        HEN-A47.
*    ＶＡＮセンターコード
     MOVE     WK-DNDT14     TO        HEN-A48.
*    取引先コード
     MOVE     WK-DNDT15     TO        HEN-A49.
*    ヘッダフラグ
     MOVE     WK-DNDT16     TO        HEN-A50.
*    ＳＥＱNO.
     MOVE     WK-DNDT17     TO        HEN-A51.
*
*    伝票NO.ＳＥＱ
     MOVE     ZERO          TO        HEN-A52.
*    総枚数
     MOVE     ZERO          TO        HEN-A53.
*
     PERFORM  TENSO-SEC.
*
*コーナン基本情報Ｆ作成
*    MOVE     WK-FLHD-REC   TO        JOH-F13.
*    MOVE     WK-DNHD-REC   TO        JOH-F14.
*    MOVE     WK-DNDT-REC   TO        JOH-F15.
     MOVE     WK-DNHD-REC   TO        JOH-FA.
     MOVE     WK-DNDT-REC   TO        JOH-FB.
     WRITE    JOH-REC.
*
     WRITE    HEN-REC.
     ADD      1             TO   WRT-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変換伝票転送                                    *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"   TO        S-NAME.
*取引先コード
     MOVE        HEN-A22A  TO        HEN-F01  JOH-F003.
*物流センターコード
     MOVE        HEN-A17   TO        HEN-F07  JOH-FA02.
*　店舗マスタを索引し、振分倉庫を取得する。
     MOVE        HEN-F01   TO        TEN-F52.
     MOVE        HEN-F07   TO        TEN-F011.
     PERFORM TENMS1-READ-SEC.
     IF   WK-TEN-F73  NOT  =  SPACE
          MOVE  23631      TO        WK-TOKCD
          MOVE  WK-TOKCD   TO        HEN-F01  JOH-F003
     ELSE
          MOVE  HEN-F01    TO        WK-TOKCD
     END-IF.
*    得意先マスタ検索
     MOVE    SPACE         TO       TOK-REC.
     INITIALIZE                     TOK-REC.
     MOVE    WK-TOKCD      TO       TOK-F01.
     READ    TOKMS2
         INVALID
           MOVE  SPACE     TO       TOK-REC
           INITIALIZE               TOK-REC
     END-READ.
     MOVE    TOK-F52       TO       HEN-F24.
*振分倉庫コード
*    ルート条件マスタ検索
     MOVE     ZERO         TO        INV-RUT.
     MOVE     SPACE        TO        RUT-REC.
     INITIALIZE                      RUT-REC.
     MOVE     WK-TOKCD     TO        RUT-F01.
     MOVE     HEN-A18      TO        RUT-F02.
     MOVE     HEN-A22B     TO        RUT-F03  HEN-F42.
     READ     JHMRUTL1
         INVALID
           MOVE  1         TO        INV-RUT
           MOVE  TOK-F81   TO        HEN-F48  JOH-F004
         NOT INVALID
           MOVE  RUT-F05   TO        HEN-F48  JOH-F004
     END-READ.
*伝票ナンバー
     MOVE        HEN-A21   TO        HEN-F02  JOH-FA06.
     MOVE        HEN-F02   TO        HEN-F23.
*伝票ナンバーブレイクチェック
*行_カウント
     IF (HEN-A21   =    WK-DENNO)
         ADD     1         TO        WK-GYO
         MOVE    WK-GYO    TO        HEN-F03
         IF   WK-TOKCD  =  23631
              ADD   1      TO        CNT-KENSU1
              ADD   1      TO        CNT-KENSU-D1
         ELSE
              ADD   1      TO        CNT-KENSU
              ADD   1      TO        CNT-KENSU-D
         END-IF
     ELSE
*********件数ファイル更新
         IF  CNT-MAISU  >  ZERO
         OR  CNT-MAISU1 >  ZERO
             PERFORM  JSMKENL1-WRT-SEC
         END-IF
*
         MOVE    1         TO        WK-GYO
         MOVE    WK-GYO    TO        HEN-F03
         MOVE    HEN-A21   TO        WK-DENNO
         MOVE    WK-TOKCD  TO        WK-TOKCD-BK
         MOVE    HEN-F48   TO        WK-HEN-F48
         IF   WK-TOKCD  =  23631
              ADD   1      TO        CNT-KENSU1
              MOVE  1      TO        CNT-KENSU-D1
              ADD   1      TO        CNT-MAISU1
         ELSE
              ADD   1      TO        CNT-KENSU
              MOVE  1      TO        CNT-KENSU-D
              ADD   1      TO        CNT-MAISU
         END-IF
     END-IF.
*取区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06  JOH-FC07.
*  商品変換テーブル検索
     MOVE        WK-TOKCD  TO        TBL-F01.
     MOVE        HEN-A45   TO        TBL-F02.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
         INITIALIZE                  TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09.
*発注日  (サブルーチンで西暦８桁にする）
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        HEN-A19   TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   HEN-F111 JOH-FA04
     ELSE
         MOVE    ZERO           TO   HEN-F111
     END-IF.
*納品日
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        HEN-A20   TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   HEN-F112  JOH-FA05
     ELSE
         MOVE    ZERO           TO   HEN-F112  JOH-FA05
     END-IF.
*出荷日
*****MOVE        HEN-F112  TO        HEN-F113.
*分類（部門）
     MOVE        HEN-A18   TO        HEN-F12.
*商品区分　　
     MOVE        SPACE     TO        HEN-F131.
*伝票区分　　
     MOVE        HEN-A29   TO        HEN-F132.
*伝発区分
     MOVE        9         TO        HEN-F134.
*自社商品コード
     MOVE        TBL-F031  TO        HEN-F1411  JOH-FC02.
*自社商品単品コード
     MOVE        TBL-F032  TO        HEN-F1412  JOH-FC03.
*商品名　　　
*    商品名称マスタ検索
     MOVE        SPACE     TO        MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE        TBL-F031  TO        MEI-F011.
     MOVE        TBL-F032  TO        MEI-F012.
     READ    MEIMS1
       INVALID
*        商品名
         MOVE    HEN-A36(1:15)   TO  HEN-F1421
         MOVE    HEN-A36(16:5)   TO  HEN-F1422(1:5)
         MOVE    HEN-A37(1:10)   TO  HEN-F1422(6:10)
       NOT INVALID
         MOVE    HEN-A36(1:15)   TO  HEN-F1421
         MOVE    HEN-A36(16:5)   TO  HEN-F1422(1:5)
         MOVE    HEN-A37(1:10)   TO  HEN-F1422(6:10)
     END-READ.
*数量
     MOVE        HEN-A42   TO        HEN-F15.
*単
     MOVE       "1"        TO        HEN-F16.
*原価単価
     MOVE        HEN-A43   TO        HEN-F172.
*売価単価
     MOVE        HEN-A44   TO        HEN-F173.
*原価金額
     COMPUTE     HEN-F181 = HEN-A42 *  HEN-A43.
*売価金額
     COMPUTE     HEN-F182 = HEN-A42 *  HEN-A44.
*自社得意先コード
*****IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
*        MOVE    SPACE         TO    TOK-REC
*        INITIALIZE                  TOK-REC
*        MOVE    WK-TOKCD      TO    TOK-F01
*        READ    TOKMS2
*            INVALID
*              MOVE  SPACE     TO    TOK-REC
*              INITIALIZE            TOK-REC
*        END-READ
*        MOVE    TOK-F52   TO        HEN-F24
*        MOVE    1         TO        FLG-TOK
*    ELSE
*        MOVE    TOK-F52   TO        HEN-F24
*****END-IF.
*相手商品コード
     MOVE        HEN-A39   TO        HEN-F25.
*伝票発行区分
     MOVE        9         TO        HEN-F272.
*オンライン区分
     MOVE        1         TO        HEN-F274.
*エントリー区分
     MOVE     1             TO        HEN-F275.
*付番区分
     MOVE     9             TO        HEN-F276.
*量販店区分
     MOVE     "A"           TO        HEN-F278.
*ＷＳ_
     MOVE     1             TO        HEN-F28.
*店舗名カナ
     MOVE     HEN-A27       TO        HEN-F30.
*システム日付
     MOVE     SYS-DATEW     TO        HEN-F99  JOH-FC04.
*システム時刻
     MOVE     SYS-TIME(1:6) TO                 JOH-FC05.
*登録者部門コード
     MOVE     2920          TO                 JOH-FC06.
*受信日付
     MOVE     PARA-JDATE    TO        HEN-F46  JOH-F001.
*受信時刻
     MOVE     PARA-JTIME    TO        HEN-F47  JOH-F002.
*_番
     MOVE        TBL-F08   TO        HEN-F49  JOH-FC01.
*訂正前数量
     MOVE        HEN-A42   TO        HEN-F50.
*修正原価単価
     MOVE        HEN-A43   TO        HEN-F512.
*修正売価単価
     MOVE        HEN-A44   TO        HEN-F513.
*修正原価金額
     COMPUTE     HEN-F521 = HEN-A42 *  HEN-A43.
*修正売価金額
     COMPUTE     HEN-F522 = HEN-A42 *  HEN-A43.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-TOKCD-BK   TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
     MOVE    WK-HEN-F48    TO        KEN-F04.
     READ    JSMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMKENL1-010
     END-READ.
*
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-TOKCD-BK   TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
     MOVE    WK-HEN-F48    TO        KEN-F04.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     IF  WK-TOKCD-BK  =  2363
         MOVE CNT-KENSU-D  TO        KEN-F10
     ELSE
         MOVE CNT-KENSU-D1 TO        KEN-F10
     END-IF.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JSMKENL1-WRT-EXIT.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     IF   WK-TOKCD-BK  =  2363
          ADD CNT-KENSU-D  TO        KEN-F10
          ADD 1            TO        KEN-F11
     ELSE
          ADD CNT-KENSU-D1 TO        KEN-F10
          ADD 1            TO        KEN-F11
     END-IF.
     REWRITE KEN-REC.
*
 JSMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　受領データ　ファイル出力
****************************************************************
*EDIT-JYR-SEC              SECTION.
*
*    MOVE    "EDIT-JYR-SEC" TO        S-NAME.
*
*    レコード区分
*    MOVE     WK-JRHD01     TO        KNJ-F01.
*    ファイル種別
*    MOVE     WK-JRHD02     TO        KNJ-F02.
*    データ作成日
*    MOVE     WK-JRHD03     TO        KNJ-F03.
*    発注企業コード
*    MOVE     WK-JRHD04     TO        KNJ-F04.
*    受注企業コード
*    MOVE     WK-JRHD05     TO        KNJ-F05.
*    レコード件数
*    MOVE     WK-JRHD06     TO        KNJ-F06.
*    帳票枚数
*    MOVE     WK-JRHD07     TO        KNJ-F07.
*    発注先企業名称
*    MOVE     WK-JRHD08     TO        KNJ-F08.
*    受注企業名称
*    MOVE     WK-JRHD09     TO        KNJ-F09.
*    受注企業住所
*    MOVE     WK-JRHD10     TO        KNJ-F10.
*    受領書発行区分
*    MOVE     WK-JRHD11     TO        KNJ-F11.
*    予備
*    MOVE     WK-JRHD12     TO        KNJ-F12.
*    ヘッダフラグ
*    MOVE     WK-JRHD13     TO        KNJ-F13.
*    ＳＥＱNO.
*    MOVE     WK-JRHD14     TO        KNJ-F14.
*
*    レコード区分
*    MOVE     WK-JRDT01     TO        KNJ-F15.
*    取引先コード
*    MOVE     WK-JRDT02     TO        KNJ-F16.
*    店舗コード
*    MOVE     WK-JRDT03     TO        KNJ-F17.
*    部門コード
*    MOVE     WK-JRDT04     TO        KNJ-F18.
*    納品日付
*    MOVE     WK-JRDT05     TO        KNJ-F19.
*    伝票番号
*    MOVE     WK-JRDT06     TO        KNJ-F20.
*    商品コード
*    MOVE     WK-JRDT07     TO        KNJ-F21.
*    ＪＡＮＣＤ
*    MOVE     WK-JRDT08     TO        KNJ-F22.
*    商品名
*    MOVE     WK-JRDT09     TO        KNJ-F23.
*    規格　　
*    MOVE     WK-JRDT10     TO        KNJ-F24.
*    符号　　
*    MOVE     WK-JRDT11     TO        KNJ-F25.
*    数量
*    MOVE     WK-JRDT12     TO        KNJ-F26.
*    原単価　
*    MOVE     WK-JRDT13     TO        KNJ-F27.
*    送信グループＣＤ
*    MOVE     WK-JRDT14     TO        KNJ-F28.
*    予備　　　　　　　　　　　
*    MOVE     WK-JRDT15     TO        KNJ-F29.
*    ヘッダフラグ
*    MOVE     WK-JRDT16     TO        KNJ-F30.
*    ＳＥＱ．ＮＯ．
*    MOVE     WK-JRDT17     TO        KNJ-F31.
*
*    WRITE    KNJ-REC.
*    ADD      1             TO   WRT-JYR-CNT.
*
*EDIT-JYR-EXIT.
*    EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        CNT-MAISU     >    ZERO
               IF  WK-TOKCD-BK   =    2363
*                  出荷場所件数マスタ出力
                   PERFORM  JSMKENL1-WRT-SEC
               END-IF
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT-SEC
     END-IF.
     IF        CNT-MAISU1    >    ZERO
               IF  WK-TOKCD-BK   =    23631
*                  出荷場所件数マスタ出力
                   PERFORM  JSMKENL1-WRT-SEC
               END-IF
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL2-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT        TO      IN-CNT.
     MOVE      WRT-CNT       TO      OUT-CNT.
*    MOVE      WRT-JYR-CNT   TO      OUT-JYR-CNT.
     MOVE      CNT-KENSU     TO      OUT-NIS-CNT.
     MOVE      CNT-KENSU1    TO      OUT-HIG-CNT.
*
     DISPLAY   MSG-IN        UPON    CONS.
     DISPLAY   MSG-OUT       UPON    CONS.
*    DISPLAY   MSG-JYR-OUT   UPON    CONS.
     DISPLAY   MSG-END       UPON    CONS.
*
     DISPLAY   MSG-NIS-OUT   UPON    CONS.
     DISPLAY   MSG-HIG-OUT   UPON    CONS.
*
     CLOSE     KONANPOR  JHSHENL1  KNJOHOF
               SHOTBL1   MEIMS1
               JHMRUTL1  TOKMS2
               JSMKENL1  JSMDAYL1
*              KNJYRF
               TENMS1.
*
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
         DISPLAY "## CNT-MAISU  = " CNT-MAISU  UPON CONS
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU1    >    ZERO
         DISPLAY "## CNT-MAISU1 = " CNT-MAISU1 UPON CONS
               PERFORM   VLD500-OUTPUT2-SEC
     END-IF.
     CLOSE     VLD500.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL1-WRT-SEC        SECTION.
*
     MOVE   "JSMDAYL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
*****MOVE    HEN-A22A      TO        TJS-F03.
     MOVE    2363          TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL1-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
*****MOVE    HEN-A22A      TO        TJS-F03.
     MOVE    2363          TO        TJS-F03.
     MOVE    9             TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     WRITE   TJS-REC.
     GO      TO   JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     REWRITE TJS-REC.
*
 JSMDAYL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL2-WRT-SEC        SECTION.
*
     MOVE   "JSMDAYL2-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
*****MOVE    HEN-A22A      TO        TJS-F03.
     MOVE    23631         TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL2-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
*****MOVE    HEN-A22A      TO        TJS-F03.
     MOVE    23631         TO        TJS-F03.
     MOVE    9             TO        TJS-F04.
     MOVE    CNT-KENSU1    TO        TJS-F09.
     MOVE    CNT-MAISU1    TO        TJS-F10.
     WRITE   TJS-REC.
     GO      TO   JSMDAYL2-WRT-EXIT.
*
 JSMDAYL2-010.
*
     MOVE    CNT-KENSU1    TO        TJS-F09.
     MOVE    CNT-MAISU1    TO        TJS-F10.
     REWRITE TJS-REC.
*
 JSMDAYL2-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD500-OUTPUT-SEC       SECTION.
*
     MOVE   "VLD500-OUTPUT-SEC" TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      PARA-JTIME         TO    VLD-F12.
     MOVE      2363               TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD500-OUTPUT2-SEC      SECTION.
*
     MOVE   "VLD500-OUTPUT2-SEC" TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      PARA-JTIME         TO    VLD-F12.
     MOVE      23631              TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT2-EXIT.
     EXIT.
****************************************************************
*　　店舗マスタ索引
****************************************************************
 TENMS1-READ-SEC         SECTION.
*
     MOVE   "TENMS1-READ-SEC"     TO   S-NAME.
*
     READ   TENMS1
            INVALID      MOVE  "INV"   TO  TENMS1-INV-FLG
                         MOVE  SPACE   TO  WK-TEN-F73
            NOT  INVALID MOVE  SPACE   TO  TENMS1-INV-FLG
                         MOVE  TEN-F73 TO  WK-TEN-F73
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
