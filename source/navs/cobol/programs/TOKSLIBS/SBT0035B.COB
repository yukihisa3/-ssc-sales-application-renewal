****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ジュンテンドーＴＣ発注システム　　*
*    モジュール名　　　　：　ＴＣデータ⇒売上基本データ作成　　*
*    作成日／更新日　　　：　2012/10/11                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＴＣ発注データより売上伝票Ｆ（物　*
*                            流用）を作成する。　　            *
*    作成日／更新日　　　：　2013/02/19                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　便区分、荷姿区分をＴＣ伝票Ｆ内に　*
*                            追加する。　　　　　　            *
*    作成日／更新日　　　：　2013/03/05                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　伝票番号を行＋伝票番号から、伝票　*
*                            番号＋行に変更する。　            *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBT0035B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/10/11.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT
     YA          IS     PITCH-2
     YB          IS     PITCH-15
     YB-21       IS     BAIKAKU.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＴＣ発注ヘッダーファイル  >>---*
     SELECT   JTCHEDL1  ASSIGN    TO        DA-01-VI-JTCHEDL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   HED-F011 HED-F012
                                                 HED-F013 HED-F02
                        FILE      STATUS    IS   HED-ST.
*---<<  ＴＣ発注明細ファイル  >>---*
     SELECT   JTCMEIL1  ASSIGN              DA-01-VI-JTCMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011 MEI-F012
                                                 MEI-F013 MEI-F02
                                                 MEI-F03
                        FILE      STATUS    IS   MEI-ST.
*---<<  ＴＣ発注明細オプションファイル  >>---*
     SELECT   JTCMEOL1  ASSIGN              DA-01-VI-JTCMEOL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   MEO-F011 MEO-F012
                                                 MEO-F013 MEO-F02
                                                 MEO-F03
                        FILE      STATUS    IS   MEO-ST.
*---<<  ＴＣ発注変換データ  >>---*
     SELECT   JTCDENL1  ASSIGN    TO        DA-01-VI-JTCDENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HEN-F46  HEN-F47
                                                 HEN-F01  HEN-F48
                                                 HEN-F02  HEN-F04
                                                 HEN-F051 HEN-F07
                                                 HEN-F112 HEN-F03
                        FILE      STATUS    IS   HEN-ST.
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
                        RECORD    KEY       IS   MES-F01
                        FILE      STATUS    IS   MES-ST.
*---<<  売上伝票修正キーＦ  >>---*
     SELECT   JTCHKYL1  ASSIGN    TO        DA-01-VI-JTCHKYL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HKY-F01  HKY-F02
                                                 HKY-F03  HKY-F04
                                                 HKY-F05  HKY-F06
                                                 HKY-F07  HKY-F08
                                                 HKY-F09  HKY-F10
                        FILE      STATUS    IS   HKY-ST.
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＴＣ発注ヘッダーファイル  >>---*
 FD  JTCHEDL1.
     COPY        JTCHEDF   OF        XFDLIB
                 JOINING   HED       PREFIX.
*---<<  ＴＣ発注明細ファイル  >>---*
 FD  JTCMEIL1.
     COPY        JTCMEIF   OF        XFDLIB
                 JOINING   MEI       PREFIX.
*---<<  ＴＣ発注明細オプションファイル  >>---*
 FD  JTCMEOL1.
     COPY     JTCMEOF   OF        XFDLIB
              JOINING   MEO       PREFIX.
*---<<  ＴＣ発注変換ファイル  >>---*
 FD  JTCDENL1.
     COPY     JTCDENF   OF        XFDLIB
              JOINING   HEN       PREFIX.
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
              JOINING   MES       PREFIX.
*---<<  売上伝票修正キーＦ  >>---* *
 FD  JTCHKYL1.
     COPY     JTCHKYF   OF        XFDLIB
              JOINING   HKY       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  HED-ST              PIC  X(02).
     03  MEI-ST              PIC  X(02).
     03  MEO-ST              PIC  X(02).
     03  HEN-ST              PIC  X(02).
     03  TOK-ST              PIC  X(02).
     03  TEN-ST              PIC  X(02).
     03  SHO-ST              PIC  X(02).
     03  MES-ST              PIC  X(02).
     03  HKY-ST              PIC  X(02).
*カウント
 01  CRT-CNT                 PIC  9(07)  VALUE ZERO.
 01  REW-CNT                 PIC  9(07)  VALUE ZERO.
 01  KEY-CNT                 PIC  9(07)  VALUE ZERO.
*頁カウント
 01  WK-PAGE                 PIC  9(03)  VALUE ZERO.
*行カウント
 01  GYO-CNT                 PIC  9(02)  VALUE ZERO.
*添字
 01  IX                      PIC  9(02)  VALUE ZERO.
*店舗ＣＤ編集
 01  WK-TENPO                PIC  9(05)  VALUE ZERO.
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
     03  MEI-END-FLG         PIC  X(03)  VALUE SPACE.
     03  MEO-END-FLG         PIC  X(03)  VALUE SPACE.
     03  SHTDENF-INV2-FLG    PIC  X(03)  VALUE SPACE.
     03  JHMTJSF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  JTCDENF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  JTCHKYF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  SURYO-CHK-FLG       PIC  X(01)  VALUE SPACE.
*計算領域
 01  WRK-AREA.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-SURYO           PIC S9(09)V9(02)  VALUE ZERO.
*伝票番号編集
 01  WRK-DENNO.
*****03  WRK-DENNO-1         PIC  9(03)  VALUE ZERO.
*****03  WRK-DENNO-2         PIC  9(06)  VALUE ZERO.
*2013/03/05 NAV ST
     03  WRK-DENNO-1         PIC  9(07)  VALUE ZERO.
     03  WRK-DENNO-2         PIC  9(02)  VALUE ZERO.
*2013/03/05 NAV END
*---<<  日付・時間ワーク  追加  96/07/29  >>---*
 01  SYS-DATE                PIC  9(06)  VALUE ZERO.
 01  HEN-DATE                PIC  9(08)  VALUE ZERO.
 01  SYS-TIME                PIC  9(08)  VALUE ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  HED-ERR           PIC N(15) VALUE
                        NC"ＴＣ発注ヘッダーエラー".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"ＴＣ発注明細エラー".
     03  MEO-ERR           PIC N(15) VALUE
                        NC"ＴＣ発注明細ＯＰエラー".
     03  HEN-ERR           PIC N(15) VALUE
                        NC"ＴＣ発注変換エラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先Ｍエラー".
     03  TEN-ERR           PIC N(15) VALUE
                        NC"店舗Ｍエラー".
     03  SHO-ERR           PIC N(15) VALUE
                        NC"商品変換Ｔエラー".
     03  MES-ERR           PIC N(15) VALUE
                        NC"商品名称Ｍエラー".
     03  HKY-ERR           PIC N(15) VALUE
                        NC"ＴＣ伝票修正キーＦエラー".
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
           NC"【　ＴＣ発注計上時　　変換エラーリスト　】".
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
 HED-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JTCHEDL1.
     MOVE        HED-ST      TO        E-ST.
     MOVE        "JTCHEDL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     HED-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JTCMEIL1.
     MOVE        MEI-ST      TO        E-ST.
     MOVE        "JTCMEIL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     MEI-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 MEO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JTCMEOL1.
     MOVE        MEO-ST      TO        E-ST.
     MOVE        "JTCMEOL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     MEO-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 HEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JTCDENL1.
     MOVE        HEN-ST      TO        E-ST.
     MOVE        "JTCDENL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     HEN-ERR     UPON      CONS.
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
 MES-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     MOVE        MES-ST      TO        E-ST.
     MOVE        "HMEIMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     MES-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 HKY-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JTCHKYL1.
     MOVE        HKY-ST      TO        E-ST.
     MOVE        "JTCHKYL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     HKY-ERR     UPON      CONS.
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
     OPEN     INPUT     JTCHEDL1.
     OPEN     INPUT     HTOKMS HTENMS HSHOTBL HMEIMS.
     OPEN     INPUT     JTCMEIL1 JTCMEOL1.
     OPEN     I-O       JTCDENL1 JTCHKYL1.
*エラーリスト印字エリア明細クリア
     MOVE     SPACE               TO   LIST-D.
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
*END-FLG CLEAR
     MOVE      SPACE         TO     END-FLG.
*ＴＣ発注ヘッダーファイルスタート
     MOVE      SPACE         TO     HED-REC.
     INITIALIZE                     HED-REC.
     MOVE      LINK-HIDUKE   TO     HED-F011.
     MOVE      LINK-JIKAN    TO     HED-F012.
     MOVE      LINK-TORICD   TO     HED-F013.
     MOVE      ZERO          TO     HED-F02.
     START     JTCHEDL1 KEY  IS  >=   HED-F011 HED-F012 HED-F013
                                      HED-F02
      INVALID
         MOVE     "END"      TO     END-FLG
         DISPLAY NC"＃在庫引当エラー（ＳＴ）＃" UPON CONS
         DISPLAY NC"＃受信日付" " = " LINK-HIDUKE UPON CONS
         DISPLAY NC"＃受信時間" " = " LINK-JIKAN  UPON CONS
         DISPLAY NC"＃取引先　" " = " LINK-TORICD UPON CONS
         GO                  TO     INIT-END
      NOT  INVALID
         PERFORM   JTCHEDL1-READ-SEC
         IF   END-FLG = "END"
              DISPLAY NC"＃在庫引当エラー（ヘＲ）＃" UPON CONS
              DISPLAY NC"＃受信日付" " = " LINK-HIDUKE UPON CONS
              DISPLAY NC"＃受信時間" " = " LINK-JIKAN  UPON CONS
              DISPLAY NC"＃取引先　" " = " LINK-TORICD UPON CONS
              GO                  TO     INIT-END
         END-IF
     END-START.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*ＴＣ発注明細オプションファイルスタート
     MOVE      SPACE         TO     MEO-REC.
     INITIALIZE                     MEO-REC.
     MOVE      HED-F011      TO     MEO-F011.
     MOVE      HED-F012      TO     MEO-F012.
     MOVE      HED-F013      TO     MEO-F013.
     MOVE      HED-F02       TO     MEO-F02.
     MOVE      ZERO          TO     MEO-F03.
     START     JTCMEOL1 KEY  IS  >=   MEO-F011 MEO-F012 MEO-F013
                                      MEO-F02  MEO-F03
      INVALID
         MOVE     "END"      TO     END-FLG
         DISPLAY NC"＃在庫引当エラー（明Ｓ）＃" UPON CONS
         DISPLAY NC"＃受信日付" " = " LINK-HIDUKE UPON CONS
         DISPLAY NC"＃受信時間" " = " LINK-JIKAN  UPON CONS
         DISPLAY NC"＃取引先　" " = " LINK-TORICD UPON CONS
         GO                  TO     MAIN-010
     END-START.
*ＴＣ発注明細Ｆ読込み
     MOVE         SPACE      TO     MEO-END-FLG.
     PERFORM   JTCMEOL1-READ-SEC.
     IF MEO-END-FLG = "END"
     OR LINK-HIDUKE NOT = MEO-F011
     OR LINK-JIKAN  NOT = MEO-F012
     OR LINK-TORICD NOT = MEO-F013
     OR HED-F02     NOT = MEO-F02
        DISPLAY NC"＃在庫引当エラー（明Ｒ）＃" UPON CONS
        DISPLAY NC"＃受信日付" " = " LINK-HIDUKE UPON CONS
        DISPLAY NC"＃受信時間" " = " LINK-JIKAN  UPON CONS
        DISPLAY NC"＃取引先　" " = " LINK-TORICD UPON CONS
        GO                  TO     MAIN-010
     END-IF.
*同一伝票番号内の明細データ処理
     PERFORM UNTIL MEO-END-FLG = "END"
                OR LINK-HIDUKE NOT = MEO-F011
                OR LINK-JIKAN  NOT = MEO-F012
                OR LINK-TORICD NOT = MEO-F013
                OR HED-F02     NOT = MEO-F02
        MOVE    ZERO           TO    GYO-CNT
        PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 9
                PERFORM JTCMEIL1-READ-SEC
****************DISPLAY "MEI-END-FLG = " MEI-END-FLG UPON CONS
                IF   MEO-A051(IX)  NOT =  ZERO
                AND  MEI-END-FLG       =  SPACE
                     ADD   1   TO    GYO-CNT
*                    変換後売上伝票Ｆエラーチェック
                     PERFORM   ERR-CHK-SEC
*                    変換ファイルセット
                     PERFORM   HENKAN-SET-SEC
*                    ＴＣ変換ファイル追加
                     IF  SURYO-CHK-FLG = "1"
                         WRITE     HEN-REC
                         ADD       1          TO      CRT-CNT
                     END-IF
                     IF  SURYO-CHK-FLG = "2"
                         REWRITE   HEN-REC
                         ADD       1          TO      REW-CNT
                     END-IF
                END-IF
        END-PERFORM
*       次レコード読込み
        PERFORM JTCMEOL1-READ-SEC
     END-PERFORM.
*
 MAIN-010.
     PERFORM  JTCHEDL1-READ-SEC.
*
 MAIN-EXIT.
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
         MOVE    TOK-F52   TO        HED-F07
     END-IF.
*店舗マスタ存在チェック*
     PERFORM     HTENMS-READ-SEC.
     IF  HTENMS-INV-FLG  =  "INV"
         MOVE NC"店舗　マスタ　未登録" TO      L-ERR
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     END-IF.
*商品変換ＴＢＬチェック*
     PERFORM     HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG  =  "INV"
         MOVE NC"商品変換ＴＢＬ未登録" TO      L-ERR
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
         MOVE    SPACE     TO        SHO-REC
         INITIALIZE                  SHO-REC
     ELSE
         MOVE    SHO-F08   TO        MEI-F11
*        *商品名称マスタチェック*
         MOVE    SHO-F04   TO        MEI-F04   MEI-F05
         MOVE    SHO-F031  TO        MEI-F061  MES-F011
         MOVE    SHO-F032  TO        MEI-F062  MES-F012
         PERFORM  HMEIMS-READ-SEC
         IF  HMEIMS-INV-FLG  =  "INV"
             MOVE NC"商品名称マスタ未登録" TO  L-ERR
             PERFORM   ERR-EDT-SEC
             PERFORM   ERR-WRT-SEC
             MOVE      "ERR"         TO        ERR-FLG
             MOVE    SPACE           TO        MES-REC
             INITIALIZE                        MES-REC
         END-IF
*        *単価チェック*
         IF  (MEI-A10  NOT = SHO-F05) OR
             (MEI-A11  NOT = SHO-F06)
             MOVE NC"単価が違います！　　" TO  L-ERR
             IF  (MEI-A10  NOT = SHO-F05)
                 MOVE      SHO-F05   TO        L-MGEN
             END-IF
             IF  (MEI-A11  NOT = SHO-F06)
                 MOVE      SHO-F06   TO        L-MURI
             END-IF
             PERFORM   ERR-EDT-SEC
             PERFORM   ERR-WRT-SEC
         END-IF
     END-IF.
*
 ERR-CHK-EXIT.
     EXIT.
****************************************************************
*    ＴＣ変換ファイルへ項目セット                              *
****************************************************************
 HENKAN-SET-SEC         SECTION.
*
     MOVE   "HENKAN-SET-SEC" TO   S-NAME.
*レコード初期化
     MOVE    SPACE           TO   HEN-REC.
     INITIALIZE                   HEN-REC.
*取引先ＣＤ
     MOVE    HED-F013        TO   HEN-F01.
*伝票番号
*****MOVE    MEO-A02         TO   WRK-DENNO-1.
*****MOVE    HED-F02         TO   WRK-DENNO-2.
*2013/03/05 NAV ST　伝票番号＋行番号に変更する。
     MOVE    HED-F02         TO   WRK-DENNO-1.
     MOVE    MEO-A02         TO   WRK-DENNO-2.
*2013/03/05 NAV END
     MOVE    WRK-DENNO       TO   HEN-F02  HEN-F23.
*行番号
     MOVE    GYO-CNT         TO   HEN-F03.
*相殺区分
     MOVE    ZERO            TO   HEN-F04.
*伝票区分
     MOVE    40              TO   HEN-F051.
     MOVE    NC"売上伝票"    TO   HEN-F052.
*相殺区分
     MOVE    ZERO            TO   HEN-F04.
*担当者ＣＤ
     MOVE    99              TO   HEN-F06.
*店舗ＣＤ
     MOVE    MEO-A04         TO   HEN-F07.
*納品日
     MOVE    HED-A18         TO   HEN-F112.
*振分倉庫
     MOVE  HED-F06           TO   HEN-F48.
*出荷場所
     MOVE    SHO-F04         TO   HEN-F08  HEN-F09.
*発注日
     MOVE    HED-A17         TO   HEN-F111.
*出荷日（出荷日はセットしない）
*****MOVE    ZERO            TO   HEN-F113.
*受信日付
     MOVE  LINK-HIDUKE       TO   HEN-F46.
*受信時刻
     MOVE  LINK-JIKAN        TO   HEN-F47.
*分類（部門）
     MOVE    HED-A13         TO   HEN-F12.
*商品区分
     MOVE    SPACE           TO   HEN-F131.
*伝票区分
     MOVE    HED-A07         TO   HEN-F132.
*伝発区分
     MOVE    9               TO   HEN-F134.
*自社商品ＣＤ
     MOVE    SHO-F031        TO   HEN-F1411.
     MOVE    SHO-F032        TO   HEN-F1412.
*商品名称
     MOVE    MEI-A04(1:15)   TO   HEN-F1421.
     MOVE    MEI-A04(16:10)  TO   HEN-F1422.
*数量
*****MOVE    MEO-F041(IX)    TO   HEN-F15.
*荷姿区分により数量の決定
      EVALUATE  HED-A09
       WHEN  10  WHEN  12
       COMPUTE WRK-SURYO = MEO-F041(IX) * MEI-A08
       WHEN  22  WHEN  34
       COMPUTE WRK-SURYO = MEO-F041(IX) * MEI-A08
       WHEN  21  WHEN  35  WHEN  36  WHEN  37
       COMPUTE WRK-SURYO = MEO-F041(IX)
      END-EVALUATE.
*存在チェック
     PERFORM JTCDENL1-READ-SEC.
*存在チェックを行ない、存在した場合で数量が違う場合、物流連携
*ＦＬＧを解除する。存在しない場合は、そのまま更新する。
     IF  JTCDENF-INV-FLG  =  "INV"
         MOVE  "1"       TO   SURYO-CHK-FLG
     ELSE
         IF  HEN-F15  NOT =  WRK-SURYO
             MOVE  "2"   TO   SURYO-CHK-FLG
             MOVE  SPACE TO   HEN-F68
             PERFORM JTCHKYL1-READ-SEC
         ELSE
             MOVE  SPACE TO   SURYO-CHK-FLG
         END-IF
     END-IF.
*数量
     MOVE    WRK-SURYO       TO   HEN-F15.
*単
     MOVE    1               TO   HEN-F16.
*仕入単価
     MOVE    MES-F041        TO   HEN-F171.
*原価単価
     MOVE    MEI-A10         TO   HEN-F172.
*売価単価
     MOVE    MEI-A11         TO   HEN-F173.
*原価金額
     COMPUTE HEN-F181 = HEN-F15 * HEN-F172.
*売価金額
     COMPUTE HEN-F182 = HEN-F15 * HEN-F173.
*自社得意先ＣＤ
     MOVE    HED-F07         TO   HEN-F24.
*相手商品ＣＤ
     MOVE    MEI-A03         TO   HEN-F25.
*伝票発行区分
     MOVE    9               TO   HEN-F272.
*オンライン区分
     MOVE    1               TO   HEN-F274.
*エントリー区分
     MOVE    1               TO   HEN-F275.
*付番区分
     MOVE    9               TO   HEN-F276.
*量販店区分
     MOVE   "A"              TO   HEN-F278.
*ＷＳ_
     MOVE   1                TO   HEN-F28.
*変換値
*****MOVE   WK-NEN           TO   HEN-F29.
*店舗名（カナ）
     EVALUATE  HED-F05
         WHEN  954   MOVE "ﾐｷ"    TO HEN-F30
         WHEN  951   MOVE "ﾋﾛｼﾏ"  TO HEN-F30
         WHEN  OTHER MOVE ALL "*" TO HEN-F30
     END-EVALUATE.
*システム日付
     MOVE  HEN-DATE          TO   HEN-F99.
*ルート
*****MOVE  HED-A12           TO   HEN-F42.
*受信日付
     MOVE  LINK-HIDUKE       TO   HEN-F46.
*受信時刻
     MOVE  LINK-JIKAN        TO   HEN-F47.
*_番
     MOVE  SHO-F08           TO   HEN-F49.
*発注数量をセットする。
      EVALUATE  HED-A09
       WHEN  10  WHEN  12
       COMPUTE WRK-SURYO = MEO-A051(IX) * MEI-A08
       WHEN  22  WHEN  34
       COMPUTE WRK-SURYO = MEO-A051(IX) * MEI-A08
       WHEN  21  WHEN  35  WHEN  36  WHEN  37
       COMPUTE WRK-SURYO = MEO-A051(IX)
      END-EVALUATE.
*訂正前数量
     MOVE  WRK-SURYO         TO   HEN-F50.
*修正原価単価
     MOVE  MEI-A10           TO   HEN-F512.
*修正売価単価
     MOVE  MEI-A11           TO   HEN-F513.
*修正原価金額
     COMPUTE HEN-F521 = HEN-F50 * HEN-F512.
*修正売価金額
     COMPUTE HEN-F522 = HEN-F50 * HEN-F513.
*センターコードセット
     MOVE  HED-F05           TO   WK-TENPO.
     MOVE  WK-TENPO          TO   HEN-F70.
*2013/02/19 NAV ST 便区分、荷姿区分追加
     MOVE  HED-A14           TO   HEN-F71.
     MOVE  HED-A09           TO   HEN-F72.
*2013/02/19 NAV ED 便区分、荷姿区分追加
*
 HENKAN-SET-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*各ファイルのＣＬＯＳＥ
     CLOSE JTCHEDL1 JTCMEIL1 JTCDENL1  JTCHKYL1
           HTOKMS   HTENMS   HSHOTBL   HMEIMS.
*データ作成件数出力
     DISPLAY "DATA ｻｸｾｲ CNT = " CRT-CNT  UPON CONS.
     DISPLAY "DATA REWT CNT = " REW-CNT  UPON CONS.
     DISPLAY "DATA KEY  CNT = " KEY-CNT  UPON CONS.
*
 END-END.
     EXIT.
****************************************************************
*                  エラーリスト明細編集　　　                  *
****************************************************************
 ERR-EDT-SEC               SECTION.
*
     MOVE   "ERR-EDT-SEC"  TO        S-NAME.
     MOVE        MEI-F02   TO        L-DEN.
     MOVE        MEI-F013  TO        L-TOR.
     MOVE        HED-F05   TO        L-TENCD.
     EVALUATE  HED-F05
         WHEN  954   MOVE  "ﾐｷ"      TO   L-TENNM
         WHEN  951   MOVE  "ﾋﾛｼﾏ"    TO   L-TENNM
         WHEN  OTHER MOVE  ALL "*"   TO   L-TENNM
     END-EVALUATE.
     MOVE        MEI-A03   TO        L-SHOCD.
     MOVE        MEI-A04   TO        L-SHONM.
     MOVE        MEI-A10   TO        L-DGEN.
     MOVE        MEI-A11   TO        L-DURI.
 ERR-EDT-EXIT.
     EXIT.
****************************************************************
*                  エラーリスト出力　　　　　                  *
****************************************************************
 ERR-WRT-SEC               SECTION.
*
     MOVE   "ERR-WRT-SEC"  TO        S-NAME.
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
     MOVE      MEI-F013    TO     TOK-F01.
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
     MOVE      MEI-F013    TO     TEN-F52.
     MOVE      HED-F05     TO     TEN-F011.
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
     MOVE      MEI-F013    TO     SHO-F01.
     MOVE      MEI-A03     TO     SHO-F02.
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
     MOVE      SHO-F031    TO     MES-F011.
     MOVE      SHO-F032    TO     MES-F012.
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
*    ＴＣ発注ヘッダーファイル読込み　　　　　　　　　　　　　　*
****************************************************************
 JTCHEDL1-READ-SEC         SECTION.
*
     MOVE     "JTCHEDL1-READ-SEC" TO    S-NAME.
*
     READ      JTCHEDL1  NEXT  AT  END
               MOVE      "END"    TO    END-FLG
               GO                 TO    JTCHEDL1-READ-EXIT
     END-READ.
*既に計上済みの場合は対象外とする。
***  IF        HED-F09   =  1
*              GO                 TO    JTCHEDL1-READ-SEC
***  END-IF.
*
     IF        LINK-HIDUKE  NOT =  HED-F011
     OR        LINK-JIKAN   NOT =  HED-F012
     OR        LINK-TORICD  NOT =  HED-F013
               MOVE      "END"    TO    END-FLG
     END-IF.
*
 JTCHEDL1-READ-EXIT.
     EXIT.
****************************************************************
*    ＴＣ発注明細オプションファイル読込み　　　　　　　　　　　*
****************************************************************
 JTCMEOL1-READ-SEC         SECTION.
*
     MOVE     "JTCMEOL1-READ-SEC"  TO    S-NAME.
*
     READ      JTCMEOL1  NEXT  AT  END
               MOVE      "END"    TO    MEO-END-FLG
     END-READ.
*
 JTCMEOL1-READ-EXIT.
     EXIT.
****************************************************************
*    ＴＣ発注明細ファイル読込み　　　　　　　　　　　　　　　　*
****************************************************************
 JTCMEIL1-READ-SEC         SECTION.
*
     MOVE     "JTCMEIL1-READ-SEC" TO    S-NAME.
*
     MOVE      LINK-HIDUKE        TO    MEI-F011.
     MOVE      LINK-JIKAN         TO    MEI-F012.
     MOVE      LINK-TORICD        TO    MEI-F013.
     MOVE      HED-F02            TO    MEI-F02.
     MOVE      IX                 TO    MEI-F03.
     READ      JTCMEIL1  INVALID
               MOVE      "END"    TO    MEI-END-FLG
               NOT  INVALID
               MOVE      SPACE    TO    MEI-END-FLG
     END-READ.
*
 JTCMEIL1-READ-EXIT.
     EXIT.
****************************************************************
*    出力ファイル存在チェック　　　　　　　　　　　　　　　　　*
****************************************************************
 JTCDENL1-READ-SEC         SECTION.
*
     MOVE     "JTCDENL1-READ-SEC" TO    S-NAME.
*
     READ      JTCDENL1  INVALID
               MOVE      "INV"    TO    JTCDENF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    JTCDENF-INV-FLG
     END-READ.
*
 JTCDENL1-READ-EXIT.
     EXIT.
****************************************************************
*    売上伝票修正キーファイル更新　　　　　　　　　　　　　　　*
****************************************************************
 JTCHKYL1-READ-SEC         SECTION.
*
     MOVE     "JTCHKYL1-READ-SEC" TO    S-NAME.
*
     MOVE     HEN-F46             TO    HKY-F01.
     MOVE     HEN-F47             TO    HKY-F02.
     MOVE     HEN-F01             TO    HKY-F03.
     MOVE     HEN-F48             TO    HKY-F04.
     MOVE     HEN-F02             TO    HKY-F05.
     MOVE     HEN-F04             TO    HKY-F06.
     MOVE     HEN-F051            TO    HKY-F07.
     MOVE     HEN-F07             TO    HKY-F08.
     MOVE     HEN-F112            TO    HKY-F09.
     MOVE     ZERO                TO    HKY-F10.
*
     READ      JTCHKYL1  INVALID
               MOVE      "INV"    TO    JTCHKYF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    JTCHKYF-INV-FLG
     END-READ.
     DISPLAY "JTCHKYF-INV-FLG = " JTCHKYF-INV-FLG UPON CONS.
*ＩＮＶの場合、売上伝票修正キーファイルを作成する。
     IF JTCHKYF-INV-FLG  =  "INV"
        MOVE     SPACE            TO    HKY-REC
        INITIALIZE                      HKY-REC
        MOVE     HEN-F46          TO    HKY-F01
        MOVE     HEN-F47          TO    HKY-F02
        MOVE     HEN-F01          TO    HKY-F03
        MOVE     HEN-F48          TO    HKY-F04
        MOVE     HEN-F02          TO    HKY-F05
        MOVE     HEN-F04          TO    HKY-F06
        MOVE     HEN-F051         TO    HKY-F07
        MOVE     HEN-F07          TO    HKY-F08
        MOVE     HEN-F112         TO    HKY-F09
        MOVE     ZERO             TO    HKY-F10
        WRITE  HKY-REC
        ADD      1                TO    KEY-CNT
     END-IF.
*
 JTCHKYL1-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
