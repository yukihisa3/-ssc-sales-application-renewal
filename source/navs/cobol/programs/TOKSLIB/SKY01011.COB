****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受配信管理システム　　　　　　　　*
*    モジュール名　　　　：　在庫引当／売上更新                *
*    作成日／更新日　　　：　1999/09/27                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　１．取引先Ｍ存在チェック          *
*                            ２．店舗Ｍ存在チェック            *
*                            ３．商品名称Ｍ存在チェック        *
*                            ４．商品変換テーブル存在チェック  *
*                            ５．エラー時、エラーリスト出力    *
*                            ６．商品在庫Ｍ引当済数量更新      *
*                            ７．売上伝票Ｆ更新                *
*                            ８．当日スケジュールマスタ更新    *
*                            ９．実行制御Ｆ更新                *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY0101B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          93/02/08.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT
     YA          IS     PITCH-2
     YB          IS     PITCH-15
     YB-21       IS     BAIKAKU.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＶＬＤ６００  >>---*
     SELECT   VLD600    ASSIGN    TO        VLD600
                        FILE      STATUS    IS    VLD-ST.
*---<<  オンライン変換後売上伝票データ  >>---*
     SELECT   JHSHENF   ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        IS    INDEXED
                        ACCESS    MODE      IS    SEQUENTIAL
                        RECORD    KEY       IS    HEN-F46
                                                  HEN-F47  HEN-F01
                                                  HEN-F02  HEN-F03
                        FILE      STATUS    IS    HEN-ST.
*---<<  特販部売上伝票ファイル  >>---*
     SELECT   SHTDENF   ASSIGN              DA-01-VI-SHTDENLA
                        ORGANIZATION        IS    INDEXED
                        ACCESS    MODE      IS    RANDOM
                        RECORD    KEY       IS    DEN-F46  DEN-F47
                                                  DEN-F01  DEN-F48
                                                  DEN-F02  DEN-F04
                                                  DEN-F051 DEN-F03
                        FILE      STATUS    IS    DEN-ST.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZZAIMS    ASSIGN    TO        DA-01-VI-ZZAIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-ST.
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-ST.
*---<<  店舗マスタ  >>---*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52 TEN-F011
                        FILE      STATUS    IS   TEN-ST.
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-ST.
*---<<  当日スケジュールマスタ  >>---*
     SELECT  JHMTJSF   ASSIGN     TO        DA-01-VI-JHMTJSL1
                       ORGANIZATION         IS   INDEXED
                       ACCESS     MODE      IS   RANDOM
                       RECORD     KEY       IS   TJS-F01
                                                 TJS-F02
                                                 TJS-F03
                       FILE       STATUS    IS   TJS-ST.
*---<<  プリンタ  >>---*
     SELECT   PRTF     ASSIGN               LP-04-PRTF
                       FILE      STATUS     IS   PRT-ST.
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＶＬＤ５００  >>---*
 FD  VLD600.
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
*---<<  変換後売上伝票ファイル  >>---*
 FD  JHSHENF.
     COPY        JHSHENF   OF        XFDLIB
                 JOINING   HEN       PREFIX.
*---<<  ＯＵＴファイル  >>---*
 FD  SHTDENF.
     COPY        SHTDENF   OF        XFDLIB
                 JOINING   DEN       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZZAIMS.
     COPY     ZZAIMS    OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  店舗マスタ  >>---*
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  当日スケジュールマスタ  >>---*
 FD  JHMTJSF.
     COPY     JHMTJSF   OF        XFDLIB
              JOINING   TJS       PREFIX.
*---<<  プリンタ >>---*
 FD  PRTF
     LABEL    RECORD     IS        OMITTED
     LINAGE              IS        66.
 01  PRT-REC            PIC  X(200).

*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  VLD-ST              PIC  X(02).
     03  HEN-ST              PIC  X(02).
     03  DEN-ST              PIC  X(02).
     03  ZAI-ST              PIC  X(02).
     03  TOK-ST              PIC  X(02).
     03  TEN-ST              PIC  X(02).
     03  SHO-ST              PIC  X(02).
     03  MEI-ST              PIC  X(02).
     03  TJS-ST              PIC  X(02).
     03  PRT-ST              PIC  X(02).
*頁カウント
 01  WK-PAGE                 PIC  9(03)  VALUE ZERO.
*フラグワーク
 01  FLG-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  CHK-FLG             PIC  X(03)  VALUE SPACE.
     03  ERR-FLG             PIC  X(03)  VALUE SPACE.
     03  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  HTENMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  JHMTJSF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  JHMJIKF-INV-FLG     PIC  X(03)  VALUE SPACE.
*計算領域
 01  WRK-AREA.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*---<<  日付・時間ワーク  追加  96/07/29  >>---*
 01  SYS-DATE                PIC  9(06)  VALUE ZERO.
 01  HEN-DATE                PIC  9(08)  VALUE ZERO.
 01  SYS-TIME                PIC  9(08)  VALUE ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  HEN-ERR           PIC N(15) VALUE
                        NC"変換売上伝票ＤＴエラー".
     03  DEN-ERR           PIC N(15) VALUE
                        NC"売上伝票ＤＴエラー".
     03  ZAI-ERR           PIC N(15) VALUE
                        NC"商品在庫Ｍエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先Ｍエラー".
     03  TEN-ERR           PIC N(15) VALUE
                        NC"店舗Ｍエラー".
     03  SHO-ERR           PIC N(15) VALUE
                        NC"商品変換Ｔエラー".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称Ｍエラー".
     03  TJS-ERR           PIC N(15) VALUE
                        NC"当日スケジュールＭエラー".
     03  VLD-ERR           PIC N(15) VALUE
                        NC"ＶＬＤＦエラー".
     03  PRT-ERR           PIC N(15) VALUE
                        NC"プリンタＦエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*見出し*
 01  LIST-M1.
     03  FILLER  CHARACTER TYPE      BAIKAKU.
         05  FILLER        PIC X(30) VALUE     SPACE.
         05  FILLER        PIC N(21) VALUE
           NC"【　オンラインデータ　変換エラーリスト　】".
         05  FILLER        PIC X(07) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC N(03) VALUE
           NC"処理日".
         05  FILLER        PIC X(01) VALUE
             ":".
         05  LSYS-YY       PIC 9(02).
         05  FILLER        PIC X(01) VALUE
             "/".
         05  LSYS-MM       PIC Z9.
         05  FILLER        PIC X(01) VALUE
             "/".
         05  LSYS-DD       PIC Z9.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(01) VALUE
           NC"頁".
         05  FILLER        PIC X(01) VALUE
             ":".
         05  LPAGE         PIC ZZ9.
 01  LIST-M2.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"伝票_".
         05  FILLER        PIC X(04) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"取引先".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(01) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-15.
         05  FILLER        PIC N(02) VALUE
           NC"店舗".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"店舗名".
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC N(05) VALUE
           NC"取引先商品".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(05) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC N(03) VALUE
           NC"商品名".
         05  FILLER        PIC X(17) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｄ原単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｍ原単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｄ売単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｍ売単価".
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(05) VALUE
           NC"エラー内容".
*明細行*
 01  LIST-D.
     03  FILLER  CHARACTER TYPE      PITCH-15.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  L-DEN         PIC 9(09).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TOR         PIC 9(08).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TENCD       PIC 9(05).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TENNM       PIC X(05).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-SHOCD       PIC X(13).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-SHONM       PIC X(18).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-DGEN        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-MGEN        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-DURI        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-MURI        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-ERR         PIC N(10).
 01  DUMMY                 PIC X(01) VALUE     SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
 LINKAGE               SECTION.
 01  LINK-HIDUKE           PIC 9(08).
 01  LINK-JIKAN            PIC 9(04).
 01  LINK-TORICD           PIC 9(08).
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION  USING  LINK-HIDUKE
                                         LINK-JIKAN
                                         LINK-TORICD.
**
 DECLARATIVES.
 VLD-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE VLD600.
     MOVE        VLD-ST      TO        E-ST.
     MOVE        "VLD600"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     VLD-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 HEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHSHENF.
     MOVE        HEN-ST      TO        E-ST.
     MOVE        "JHSHENF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     HEN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENF.
     MOVE        DEN-ST      TO        E-ST.
     MOVE        "SHTDENF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     DEN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZZAIMS.
     MOVE        ZAI-ST      TO        E-ST.
     MOVE        "ZZAIMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     ZAI-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST      TO        E-ST.
     MOVE        "HTOKMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     TOK-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTENMS.
     MOVE        TEN-ST      TO        E-ST.
     MOVE        "HTENMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     TEN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     MOVE        SHO-ST      TO        E-ST.
     MOVE        "HSHOTBL"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     SHO-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST      TO        E-ST.
     MOVE        "HMEIMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     MEI-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 TJS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHMTJSF.
     MOVE        TJS-ST      TO        E-ST.
     MOVE        "JHMTJSF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     TJS-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE PRTF.
     MOVE        PRT-ST      TO        E-ST.
     MOVE        "PRTF   "   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     PRT-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 END     DECLARATIVES.
****************************************************************
 PROCESS-START               SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC  UNTIL   END-FLG  =  "END".
     PERFORM       END-SEC.
     STOP      RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       ZZAIMS   SHTDENF  JHSHENF.
     OPEN     INPUT     HTOKMS   HTENMS   HSHOTBL   HMEIMS.
     OPEN     OUTPUT    VLD600.
*システム日付／時刻取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   HEN-DATE.
     ACCEPT    SYS-TIME         FROM   TIME.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*END-FLG CLEAR
     MOVE      SPACE         TO     END-FLG.
*変換後売上伝票ファイルスタート
     MOVE      LINK-HIDUKE   TO     HEN-F46.
     MOVE      LINK-JIKAN    TO     HEN-F47.
     MOVE      LINK-TORICD   TO     HEN-F01.
     MOVE      ZERO          TO     HEN-F02.
     MOVE      ZERO          TO     HEN-F03.
     START     JHSHENF  KEY  IS  >=   HEN-F46 HEN-F47 HEN-F01
                                      HEN-F02 HEN-F03
      INVALID
         MOVE     "END"      TO     END-FLG
         DISPLAY NC"＃伝票更新エラー（ＳＴ）＃" UPON CONS
         DISPLAY NC"＃受信日付" " = " LINK-HIDUKE UPON CONS
         DISPLAY NC"＃受信時間" " = " LINK-JIKAN  UPON CONS
         DISPLAY NC"＃取引先　" " = " LINK-TORICD UPON CONS
         GO                  TO     MAIN010
      NOT  INVALID
         PERFORM   JHSHENF-READ-SEC
         IF   END-FLG = "END"
              DISPLAY NC"＃伝票更新エラー（ＳＴ）＃" UPON CONS
              DISPLAY NC"＃受信日付" " = " LINK-HIDUKE UPON CONS
              DISPLAY NC"＃受信時間" " = " LINK-JIKAN  UPON CONS
              DISPLAY NC"＃取引先　" " = " LINK-TORICD UPON CONS
              GO                  TO     MAIN010
         END-IF
     END-START.
*
     OPEN     OUTPUT    PRTF.
     PERFORM   SHTDENF-CRT-SEC  UNTIL  END-FLG =  "END".
     CLOSE              PRTF.
*当日スケジュールマスタ更新
     PERFORM JHMTJSF-REWRITE-SEC.
 MAIN010.
*ＶＬＤ６００終了更新
     PERFORM   VLD600-WRITE-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*      2.1.1　売上伝票ファイル作成                            *
****************************************************************
 SHTDENF-CRT-SEC        SECTION.
*
     MOVE     "SHTDENF-CRT-SEC"   TO   S-NAME.
*フラグ初期化
     INITIALIZE         KEP-FLG  ERR-FLG  CHK-FLG.
*変換後売上伝票Ｆエラーチェック
     PERFORM   ERR-CHK-SEC.
*二重計上チェック（二重計上の場合は更新済みにする）
     IF  CHK-FLG  =  "CHK"
         MOVE         1       TO     HEN-F54
         MOVE         1       TO     HEN-F55
*        エラーフラグ更新へ
         REWRITE      HEN-REC
         GO                   TO     CHK010
     END-IF.
*在庫引当処理
     IF  HSHOTBL-INV-FLG  =  SPACE
         PERFORM  ZAIKO-SEC
         IF  KEP-FLG  =  SPACE
             MOVE     1       TO     HEN-F27D
         END-IF
         MOVE         1       TO     HEN-F262
     END-IF.
*エラー時、エラー区分セット
     IF  ERR-FLG  =  "ERR"
         MOVE         1       TO     HEN-F55
     END-IF.
*更新済みＦＬＧセット
     MOVE          1          TO     HEN-F54.
*変換後レコードセット
     MOVE          HEN-REC    TO     DEN-REC.
*エラーフラグ更新へ
     REWRITE       HEN-REC.
*売上伝票ファイル書き込み
     WRITE         DEN-REC.
 CHK010.
*売上伝票ファイル読込み
     PERFORM       JHSHENF-READ-SEC.
*
 SHTDENF-CHK-EXIT.
     EXIT.
****************************************************************
*      2.1.1　売上伝票ファイル作成                            *
****************************************************************
 ERR-CHK-SEC            SECTION.
*
     MOVE     "ERR-CHK-SEC"       TO   S-NAME.
*取引先マスタ存在チェック*
     PERFORM     HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  =  "INV"
         MOVE NC"取引先マスタ　未登録" TO      L-ERR
         PERFORM           ERR-EDT-SEC
         PERFORM           ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     ELSE
         MOVE    TOK-F52   TO        HEN-F24
         MOVE    TOK-F86   TO        HEN-F279  HEN-F27A
     END-IF.
*店舗マスタ存在チェック*
     PERFORM     HTENMS-READ-SEC.
     IF  HTENMS-INV-FLG  =  "INV"
         MOVE NC"店舗　マスタ　未登録" TO      L-ERR
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     ELSE
         MOVE    99        TO        HEN-F06
     END-IF.
*商品変換ＴＢＬチェック*
     PERFORM     HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG  =  "INV"
         MOVE NC"商品変換ＴＢＬ未登録" TO      L-ERR
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     ELSE
*        *商品名称マスタチェック*
         MOVE    SHO-F04   TO        HEN-F08   HEN-F09
         MOVE    SHO-F031  TO        HEN-F1411 MEI-F011
         MOVE    SHO-F032  TO        HEN-F1412 MEI-F012
         PERFORM  HMEIMS-READ-SEC
         IF  HMEIMS-INV-FLG  =  "INV"
             MOVE NC"商品名称マスタ未登録" TO  L-ERR
             PERFORM   ERR-EDT-SEC
             PERFORM   ERR-WRT-SEC
             MOVE      "ERR"         TO        ERR-FLG
         ELSE
             MOVE          MEI-F031  TO        HEN-F1421
             MOVE          MEI-F032  TO        HEN-F1422
             MOVE          MEI-F041  TO        HEN-F171
         END-IF
*        *単価チェック*
         IF  (HEN-F172 NOT = SHO-F05) OR
             (HEN-F173 NOT = SHO-F06)
             MOVE NC"単価が違います！　　" TO  L-ERR
             IF  (DEN-F172 NOT = SHO-F05)
                 MOVE      SHO-F05   TO        L-MGEN
             END-IF
             IF  (DEN-F173 NOT = SHO-F06)
                 MOVE      SHO-F06   TO        L-MURI
             END-IF
             PERFORM   ERR-EDT-SEC
             PERFORM   ERR-WRT-SEC
         END-IF
     END-IF.
*売上伝票ファイル存在チェック（二重計上防止）
     PERFORM  SHTDENF-READ-SEC.
     IF  SHTDENF-INV-FLG  =  SPACE
         MOVE NC"売上二重計上"         TO      L-ERR
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "CHK"     TO        CHK-FLG
     END-IF.
*
 ERR-CHK-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    HEN-F08         TO   ZAI-F01.
     MOVE    HEN-F1411       TO   ZAI-F021.
     MOVE    HEN-F1412       TO   ZAI-F022.
     MOVE    SHO-F08         TO   ZAI-F03.
     READ    ZZAIMS
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
      MOVE      "1"           TO   KEP-FLG.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      HEN-F08       TO   ZAI-F01.
      MOVE      HEN-F1411     TO   ZAI-F021.
      MOVE      HEN-F1412     TO   ZAI-F022.
      MOVE      SHO-F08       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F13       =    ZAI-F13  +  HEN-F15.
*商品名称マスタ読込み
      PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
      IF  HMEIMS-INV-FLG  =  SPACE
          MOVE  MEI-F031      TO   ZAI-F04
          WRITE ZAI-REC
      END-IF.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F06  -  ZAI-F14.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  HEN-F15.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F13   =   ZAI-F13  +  HEN-F15
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
*        引当済数に数量加算
         COMPUTE  ZAI-F14   =   ZAI-F14  +  HEN-F15
*        未出庫数に数量加算
         COMPUTE  ZAI-F13   =   ZAI-F13  +  HEN-F15
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*各ファイルのＣＬＯＳＥ
     CLOSE    JHSHENF SHTDENF ZZAIMS HTOKMS HTENMS HSHOTBL HMEIMS.
     CLOSE    VLD600.
     DISPLAY NC"＃＃売上更新処理終了＃＃" UPON CONS.
*
 END-END.
     EXIT.
****************************************************************
*                  エラーリスト明細編集　　　                  *
****************************************************************
 ERR-EDT-SEC               SECTION.
*
     MOVE   "ERR-EDT-SEC"  TO        S-NAME.
     MOVE        HEN-F02   TO        L-DEN.
     MOVE        HEN-F01   TO        L-TOR.
     MOVE        HEN-F07   TO        L-TENCD.
     MOVE        HEN-F30   TO        L-TENNM.
     MOVE        HEN-F25   TO        L-SHOCD.
     MOVE        HEN-F1421 TO        L-SHONM.
     MOVE        HEN-F1422(1:3) TO   L-SHONM(16:3).
     MOVE        HEN-F172  TO        L-DGEN.
     MOVE        HEN-F173  TO        L-DURI.
***  DISPLAY "L-DEN      = " L-DEN        UPON CONS.
***  DISPLAY "L-TOR      = " L-TOR        UPON CONS.
***  DISPLAY "L-TENCD    = " L-TENCD      UPON CONS.
***  DISPLAY "L-TENNM    = " L-TENNM      UPON CONS.
***  DISPLAY "L-SHOCD    = " L-SHOCD      UPON CONS.
***  DISPLAY "L-SHONM    = " L-SHONM      UPON CONS.
***  DISPLAY "L-DGEN     = " L-DGEN       UPON CONS.
***  DISPLAY "L-DURI     = " L-DURI       UPON CONS.
***  DISPLAY "HEN-F02    = " HEN-F02      UPON CONS.
***  DISPLAY "HEN-F01    = " HEN-F01      UPON CONS.
***  DISPLAY "HEN-F07    = " HEN-F07      UPON CONS.
***  DISPLAY "HEN-F30    = " HEN-F30      UPON CONS.
***  DISPLAY "HEN-F25    = " HEN-F25      UPON CONS.
***  DISPLAY "HEN-F1421  = " HEN-F1421    UPON CONS.
***  DISPLAY "HEN-F1422  = " HEN-F1422    UPON CONS.
***  DISPLAY "HEN-F172   = " HEN-F172     UPON CONS.
***  DISPLAY "HEN-F173   = " HEN-F173     UPON CONS.
 ERR-EDT-EXIT.
     EXIT.
****************************************************************
*                  エラーリスト出力　　　　　                  *
****************************************************************
 ERR-WRT-SEC               SECTION.
*
     MOVE   "ERR-WRT-SEC"  TO        S-NAME.
     IF  (WK-PAGE = ZERO) OR (LINAGE-COUNTER > 61)
         IF  (LINAGE-COUNTER > 61)
             MOVE          SPACE     TO        PRT-REC
             WRITE         PRT-REC   AFTER     PAGE
         END-IF
         ADD     1         TO        WK-PAGE
         MOVE    WK-PAGE   TO        LPAGE
         MOVE    HEN-DATE(1:4)  TO        LSYS-YY
         MOVE    HEN-DATE(5:2)  TO        LSYS-MM
         MOVE    HEN-DATE(7:2)  TO        LSYS-DD
         WRITE   PRT-REC   FROM      LIST-M1   AFTER     1
         WRITE   PRT-REC   FROM      LIST-M2   AFTER     2
         WRITE   PRT-REC   FROM      DUMMY     AFTER     1
     END-IF.
     WRITE       PRT-REC   FROM      LIST-D    AFTER     1.
     MOVE        SPACE     TO        LIST-D.
 ERR-WRT-EXIT.
     EXIT.
****************************************************************
*                得意先マスタ読込み                            *
****************************************************************
 HTOKMS-READ-SEC           SECTION.
*
     MOVE      "HTOKMS-READ-SEC"  TO    S-NAME.
*
     MOVE      HEN-F01     TO     TOK-F01.
     READ      HTOKMS
               INVALID
               MOVE      "INV"    TO    HTOKMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*                店舗マスタ読込み                              *
****************************************************************
 HTENMS-READ-SEC           SECTION.
*
     MOVE      "HTENMS-READ-SEC"  TO    S-NAME.
*
     MOVE      HEN-F01     TO     TEN-F52.
     MOVE      HEN-F07     TO     TEN-F011.
     READ      HTENMS
               INVALID
               MOVE      "INV"    TO    HTENMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      HEN-F01     TO     SHO-F01.
     MOVE      HEN-F25     TO     SHO-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
*
     MOVE      SHO-F031    TO     MEI-F011.
     MOVE      SHO-F032    TO     MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*                売上伝票ファイル読込み                        *
****************************************************************
 SHTDENF-READ-SEC          SECTION.
*
     MOVE      "SHTDENF-READ-SEC" TO    S-NAME.
*
     MOVE      HEN-F46     TO     DEN-F46.
     MOVE      HEN-F47     TO     DEN-F47.
     MOVE      HEN-F01     TO     DEN-F01.
     MOVE      HEN-F48     TO     DEN-F48.
     MOVE      HEN-F02     TO     DEN-F02.
     MOVE      HEN-F04     TO     DEN-F04.
     MOVE      HEN-F051    TO     DEN-F051.
     MOVE      HEN-F03     TO     DEN-F03.
     READ      SHTDENF
               INVALID
               MOVE      "INV"    TO    SHTDENF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SHTDENF-INV-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*                変換後売上伝票ファイル読込み                  *
****************************************************************
 JHSHENF-READ-SEC          SECTION.
*
     MOVE      "JHSHENF-READ-SEC" TO    S-NAME.
*
     READ      JHSHENF  AT  END
               MOVE     "END"     TO    END-FLG
     END-READ.
*
 JHSHENF-READ-EXIT.
     EXIT.
****************************************************************
*                ＶＬＤ６００読込み                            *
****************************************************************
 VLD600-WRITE-SEC           SECTION.
*
     MOVE      "VLD600-READ-SEC"  TO    S-NAME.
*
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      600                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      LINK-HIDUKE        TO    VLD-F11.
     MOVE      LINK-JIKAN         TO    VLD-F12.
     MOVE      LINK-TORICD        TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD600-WRITE-EXIT.
     EXIT.
****************************************************************
*                当日スケジュールマスタ更新                   *
****************************************************************
 JHMTJSF-REWRITE-SEC       SECTION.
*
     MOVE      "JHMTJSF-REWRITE-SEC" TO S-NAME.
*ファイルのＯＰＥＮ
     OPEN      I-O     JHMTJSF.
*
     MOVE      LINK-HIDUKE     TO   TJS-F01.
     MOVE      LINK-JIKAN      TO   TJS-F02.
     MOVE      LINK-TORICD     TO   TJS-F03.
*ファイル読込み
     READ      JHMTJSF
               INVALID
                 MOVE  "INV"   TO   JHMTJSF-INV-FLG
               NOT  INVALID
                 MOVE  SPACE   TO   JHMTJSF-INV-FLG
     END-READ.
*処理区分により各項目更新
     IF   JHMTJSF-INV-FLG  =  "INV"
          MOVE SPACE         TO  TJS-REC
          INITIALIZE             TJS-REC
          MOVE LINK-HIDUKE   TO  TJS-F01
          MOVE LINK-JIKAN    TO  TJS-F02
          MOVE LINK-TORICD   TO  TJS-F03
          MOVE 4             TO  TJS-F11
          WRITE TJS-REC
     ELSE
          MOVE 4             TO  TJS-F11
          REWRITE TJS-REC
     END-IF.
*ファイルのＣＬＯＳＥ
     CLOSE        JHMTJSF.
*
 JHMTJSF-REWRITE-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
