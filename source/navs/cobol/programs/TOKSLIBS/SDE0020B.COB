****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ連携　　　　　　　　*
*    モジュール名　　　　：　伝票ＥＸＣＥＬデータチェック　　　*
*    作成日／作成者　　　：　2016/09/29 INOUE                  *
*    処理内容　　　　　　：　伝票ＥＸＣＥＬデータについて、　　*
*    　　　　　　　　　　　　整合性チェック・件数カウントを　　*
*                            を行う。　　　　　　　　　　　　　*
*    流用　　　　　　　　：  SZI0030B                          *
*    変更日／作成者　　　：　2016/10/11                        *
*    変更内容　　　　　　：　量販店形式　チェック仕様変更      *
*                            　伝票番号　　　　　　　　　　　　*
*                            　伝票区分　　　　　　　　　　　　*
*    変更日／作成者　　　：　2017/01/16                        *
*    変更内容　　　　　　：　行番号＝０の備考欄の１桁目が９の  *
*                            時、備考欄に９をセットする。　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SDE0020B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*伝票ＥＸＣＥＬデータ　※ＣＳＶ形式
     SELECT      DCSVXXX     ASSIGN    TO       DA-01-S-DCSVXXX
                             ORGANIZATION       SEQUENTIAL
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   CSV-ST.
*伝票ＥＸＣＥＬ取込ファイル
     SELECT      DEXXXXL1    ASSIGN    TO       DA-01-VI-DEXXXXL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      DEX-F301
                                                DEX-F201
                                                WITH  DUPLICATES
                             FILE      STATUS   DEX-ST.
*条件ファイル
     SELECT      JYOKEN1     ASSIGN    TO       DA-01-VI-JYOKEN1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      JYO-F01 JYO-F02
                             FILE      STATUS   JYO-ST.
*商品名称マスタ
     SELECT      MEIMS1      ASSIGN    TO       DA-01-VI-MEIMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MEI-F011
                                                MEI-F0121
                                                MEI-F0122
                                                MEI-F0123
                             FILE      STATUS   MEI-ST.
*倉庫マスタ
     SELECT      ZSOKMS1     ASSIGN    TO       DA-01-VI-ZSOKMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SOK-F01
                             FILE      STATUS   SOK-ST.
*商品変換テーブル
     SELECT      SHOTBL1     ASSIGN    TO        DA-01-VI-SHOTBL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TBL-F01
                                                 TBL-F02
                             FILE  STATUS   IS   TBL-ST.
*取引先マスタ
     SELECT      TOKMS2      ASSIGN    TO        DA-01-VI-TOKMS2
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TOK-F01
                             FILE      STATUS    TOK-ST.
*店舗マスタ
     SELECT      TENMS1      ASSIGN    TO        DA-01-VI-TENMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TEN-F52
                                                 TEN-F011
                             FILE      STATUS    TEN-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*伝票ＥＸＣＥＬデータ
 FD  DCSVXXX
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    25        RECORDS.
     COPY        DCSVXXX1    OF        XFDLIB
     JOINING     CSV         AS        PREFIX.
*伝票ＥＸＣＥＬ取込ファイル
 FD  DEXXXXL1.
     COPY        DEXXXXL1    OF        XFDLIB
     JOINING     DEX         AS        PREFIX.
*条件ファイル
 FD  JYOKEN1.
     COPY        JYOKEN1     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        MEIMS1      OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS1.
     COPY        ZSOKMS1     OF        XFDLIB
     JOINING     SOK         AS        PREFIX.
*商品変換TBL
 FD  SHOTBL1.
     COPY        SHOTBL1     OF        XFDLIB
     JOINING     TBL         AS        PREFIX.
*取引先マスタ
 FD  TOKMS2.
     COPY        TOKMS2      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
*店舗マスタ
 FD  TENMS1.
     COPY        TENMS1      OF        XFDLIB
     JOINING     TEN         AS        PREFIX.
*
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  CSV-ST              PIC  X(02)  VALUE  SPACE.
     03  DEX-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
     03  TBL-ST              PIC  X(02)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
     03  TEN-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  INIT-FLG            PIC  X(01)  VALUE  "1".
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  CSV-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  CSV-CNT             PIC  9(07)  VALUE  ZERO.
     03  DEX-CNT             PIC  9(07)  VALUE  ZERO.
     03  CNT-MEISAI          PIC  9(07)  VALUE  ZERO.
     03  CNT-URIAGE          PIC  9(07)  VALUE  ZERO.
     03  CNT-HENPIN          PIC  9(07)  VALUE  ZERO.
     03  CNT-NEBIKI          PIC  9(07)  VALUE  ZERO.
     03  CNT-ERR             PIC  9(07)  VALUE  ZERO.
     03  CNT-UNKNOWN         PIC  9(07)  VALUE  ZERO.
     03  CNT-OK              PIC  9(07)  VALUE  ZERO.
     03  WK-GYO-CNT          PIC  9(02)  VALUE  ZERO.
**   03  CSV-F02-CNT         PIC  9(03)  VALUE  ZERO.
     03  SAV-CSV-F00         PIC  9(05)  VALUE  ZERO.
     03  SAV-CSV-F01         PIC  X(01)  VALUE  SPACE.
     03  SAV-CSV-F02         PIC  X(01)  VALUE  SPACE.
     03  SAV-CSV-F04         PIC  9(09)  VALUE  ZERO.
     03  SAV-CSV-F05         PIC  9(02)  VALUE  ZERO.
     03  SAV-CSV-F06         PIC  X(02)  VALUE  SPACE.
     03  SAV-CSV-F07         PIC  9(05)  VALUE  ZERO.
*行0共通項目退避用
     03  SAV-0-F06           PIC  X(02)  VALUE  SPACE.
     03  SAV-0-F07           PIC  9(05)  VALUE  ZERO.
     03  SAV-0-F08           PIC  X(02)  VALUE  SPACE.
     03  SAV-0-F09           PIC  X(02)  VALUE  SPACE.
     03  SAV-0-F10           PIC  9(08)  VALUE  ZERO.
     03  SAV-0-F11           PIC  9(08)  VALUE  ZERO.
     03  SAV-0-F12           PIC  X(04)  VALUE  SPACE.
     03  SAV-0-F13           PIC  X(02)  VALUE  SPACE.
     03  SAV-0-F14           PIC  X(02)  VALUE  SPACE.
**
     03  WK-ERR-TBL.
         05  ERR-KBN         PIC  X(01)  OCCURS  16.
     03  WK-ERR-KBN          PIC  X(01).
*
 01  WK-CSV-F05              PIC  X(04)  VALUE SPACE.
 01  WK-CSV-F05-X            REDEFINES   WK-CSV-F05.
     03  WK-CSV-F05R         PIC  9(04).
*
 01  MEI-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SOK-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  TBL-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  TOK-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  TEN-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
**
**条件ファイルＲＥＣ保管
 COPY     JYOKEN1            OF  XFDLIB
 JOINING  JYO-KEIRI-SIME     AS  PREFIX.
 COPY     JYOKEN1            OF  XFDLIB
 JOINING  JYO-KEIRI-TUKI     AS  PREFIX.
**
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
*
 01  SYS-DATE8               PIC  9(08)  VALUE  ZERO.
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
 01  WK-SAGYOUBI-1           PIC  9(08)  VALUE  ZERO.
 01  WK-SAGYOUBI-1-YMD       REDEFINES   WK-SAGYOUBI-1.
     03  WK-SAGYOUBI-Y       PIC  9(04).
     03  WK-SAGYOUBI-M       PIC  9(02).
     03  WK-SAGYOUBI-D       PIC  9(02).
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*経理〆日
 01  WK-SIME                 PIC  9(08).
*経理〆月初日
 01  WK-SIME01               PIC  9(08).
 01  WK-SIME01X              REDEFINES WK-SIME01.
     03 WK-SIME01-YYYY       PIC  9(04).
     03 WK-SIME01-MM         PIC  9(02).
     03 WK-SIME01-DD         PIC  9(02).
 01  WK-SIME01-MM-WK         PIC  9(09)V99.
*経理〆月末日
 01  WK-SIME99               PIC  9(08).
 01  WK-SIME99X              REDEFINES WK-SIME99.
     03 WK-SIME99-YYYY       PIC  9(04).
     03 WK-SIME99-MM         PIC  9(02).
     03 WK-SIME99-DD         PIC  9(02).
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  CSV-ERR            PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬデータ異常".
     03  DEX-ERR             PIC  N(10)  VALUE
                   NC"ＥＸＣＥＬ取込Ｆ異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  SOK-ERR             PIC  N(10)  VALUE
                   NC"倉庫マスタ異常".
     03  TBL-ERR             PIC  N(10)  VALUE
                   NC"商品変換ＴＢＬ異常".
     03  TOK-ERR             PIC  N(10)  VALUE
                   NC"取引先マスタ異常".
     03  TEN-ERR             PIC  N(10)  VALUE
                   NC"店舗マスタ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SDE0020B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SDE0020B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT0.
         05  MSG-OUT0-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT0-FIL2   PIC  N(11)  VALUE
                             NC"伝票ＥＸＣＥＬ総件数＝".
         05  MSG-OUT00       PIC  ZZZ,ZZ9.
         05  MSG-OUT0-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT0-FIL4   PIC  N(01)  VALUE NC"件".
*    03  MSG-OUT1.
*        05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
*        05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
*                            NC"　伝票明細総数　　　＝".
*        05  MSG-OUT01       PIC  ZZZ,ZZ9.
*        05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
*        05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"　　内．売上伝票　　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"　　内．返品伝票　　＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"　　内．値引伝票　　＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT5.
         05  MSG-OUT5-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT5-FIL2   PIC  N(11)  VALUE
                             NC"取込ファイル作成数　＝".
         05  MSG-OUT05       PIC  ZZZ,ZZ9.
         05  MSG-OUT5-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT5-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT6.
         05  MSG-OUT6-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT6-FIL2   PIC  N(11)  VALUE
                             NC"　内．エラーデータ　＝".
         05  MSG-OUT06       PIC  ZZZ,ZZ9.
         05  MSG-OUT6-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT6-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT7.
         05  MSG-OUT7-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT7-FIL2   PIC  N(11)  VALUE
                             NC"　内．伝票区分不明　＝".
         05  MSG-OUT07       PIC  ZZZ,ZZ9.
         05  MSG-OUT7-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT7-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT8.
         05  MSG-OUT8-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT8-FIL2   PIC  N(11)  VALUE
                             NC"　内．ＯＫデータ　　＝".
         05  MSG-OUT08       PIC  ZZZ,ZZ9.
         05  MSG-OUT8-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT8-FIL4   PIC  N(01)  VALUE NC"件".
*#2016/11/14 NAV ST システム日付の月初日取得
 01  WK-SYSDT-ST.
     03  WK-SYSDT-YYYY       PIC  9(04)  VALUE  ZERO.
     03  WK-SYSDT-MM         PIC  9(02)  VALUE  ZERO.
     03  WK-SYSDT-DD         PIC  9(02)  VALUE  ZERO.
*#2016/11/14 NAV ED システム日付の月初日取得
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-IN-EXCEL               PIC  X(01).
 01  LINK-OUT-EXCEL              PIC  X(01).
 01  LINK-OUT-CNT1               PIC  9(07).
 01  LINK-OUT-CNT2               PIC  9(07).
 01  LINK-OUT-CNT3               PIC  9(07).
 01  LINK-OUT-CNT4               PIC  9(07).
 01  LINK-OUT-CNT5               PIC  9(07).
 01  LINK-OUT-DATE               PIC  9(08).
 01  LINK-OUT-TIME               PIC  9(06).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-IN-EXCEL
                                             LINK-OUT-EXCEL
                                             LINK-OUT-CNT1
                                             LINK-OUT-CNT2
                                             LINK-OUT-CNT3
                                             LINK-OUT-CNT4
                                             LINK-OUT-CNT5
                                             LINK-OUT-DATE
                                             LINK-OUT-TIME.
****************************************************************
 DECLARATIVES.
 CSV-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DCSVXXX.
     DISPLAY     CSV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     CSV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 DEX-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DEXXXXL1.
     DISPLAY     DEX-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEX-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZSOKMS1.
     DISPLAY     SOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TBL-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHOTBL1.
     DISPLAY     TBL-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TBL-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE TOKMS2.
     DISPLAY     TOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE TENMS1.
     DISPLAY     TEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE       "INIT-SEC"   TO   S-NAME.
*
     DISPLAY     MSG-START   UPON  CONS.
*
     OPEN        INPUT       DCSVXXX.
     OPEN        I-O         DEXXXXL1.
     OPEN        INPUT       MEIMS1   ZSOKMS1  SHOTBL1
                             TOKMS2   TENMS1   JYOKEN1.
*
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
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
     MOVE      LINK-OUT-YMD       TO   WK-DATE8
                                       SYS-DATE8.
*#2016/11/14 NAV ST 追加
     MOVE      LINK-OUT-YMD       TO   WK-SYSDT-ST.
*#2016/11/14 NAV ED 追加
     DISPLAY "# HIDUKE = " WK-DATE8   UPON CONS.
     DISPLAY "# JIKAN  = " WK-TIME-HM UPON CONS.
*
*条件ファイルＲＥＣ事前取得（経理締め月度）
*
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY
                NC"＃＃条件ファイル取得エラー（経理締月度）＃＃"
                                                       UPON CONS
              MOVE  4001     TO   PROGRAM-STATUS
              STOP  RUN
     END-IF.
*T
*    DISPLAY "# JYO-F04= " JYO-F04          UPON CONS.
*T
*
     MOVE     JYO-REC        TO   JYO-KEIRI-SIME-REC.
*T
*    MOVE     JYO-KEIRI-SIME-F04  TO   WK-SIME01-MM-WK.
*    DISPLAY "# SIME-F04= " WK-SIME01-MM-WK  UPON CONS.
*T
*
*条件ファイルＲＥＣ事前取得（月別経理締め年月日）
*
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY
                NC"＃＃条件ファイル取得エラー（経理締め日）＃＃"
                                                       UPON CONS
              MOVE  4001     TO   PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
     MOVE     JYO-REC        TO   JYO-KEIRI-TUKI-REC.
*
*経理月の１日を求める
     MOVE     WK-DATE8            TO   WK-SIME01.
     MOVE     JYO-KEIRI-SIME-F04  TO   WK-SIME01-MM-WK.
     MOVE     WK-SIME01-MM-WK     TO   WK-SIME01-MM.
*T
     DISPLAY NC"経理月　　＝"  WK-SIME01-MM       UPON CONS.
*T
     IF     ( WK-DATE8(5:2) = 01  ) AND
            ( WK-SIME01-MM =  12  )
              COMPUTE WK-SIME01-YYYY = WK-SIME01-YYYY - 1
     END-IF.
     MOVE     01                  TO   WK-SIME01-DD.
*T
     DISPLAY NC"経理月１日＝" WK-SIME01 UPON CONS.
*T
*
*
*発注日・納品日のチェック対象となる締処理年月日を特定する
     EVALUATE   JYO-KEIRI-SIME-F04
         WHEN   01    MOVE  JYO-KEIRI-TUKI-F04   TO   WK-SIME
         WHEN   02    MOVE  JYO-KEIRI-TUKI-F05   TO   WK-SIME
         WHEN   03    MOVE  JYO-KEIRI-TUKI-F06   TO   WK-SIME
         WHEN   04    MOVE  JYO-KEIRI-TUKI-F07   TO   WK-SIME
         WHEN   05    MOVE  JYO-KEIRI-TUKI-F08   TO   WK-SIME
         WHEN   06    MOVE  JYO-KEIRI-TUKI-F09   TO   WK-SIME
         WHEN   07    MOVE  JYO-KEIRI-TUKI-F10   TO   WK-SIME
         WHEN   08    MOVE  JYO-KEIRI-TUKI-F11   TO   WK-SIME
         WHEN   09    MOVE  JYO-KEIRI-TUKI-F12   TO   WK-SIME
         WHEN   10    MOVE  JYO-KEIRI-TUKI-F12A  TO   WK-SIME
         WHEN   11    MOVE  JYO-KEIRI-TUKI-F12B  TO   WK-SIME
         WHEN   12    MOVE  JYO-KEIRI-TUKI-F12C  TO   WK-SIME
     END-EVALUATE.
*
*システム日付の翌月末日を求める
     MOVE     SYS-DATE8      TO   WK-SIME99.
     ADD      100            TO   WK-SIME99.
     IF       WK-SIME99-MM   =    13
              MOVE   01      TO   WK-SIME99-MM
              ADD    10000   TO   WK-SIME99
     END-IF.
     MOVE     99             TO   WK-SIME99-DD.
*T
     DISPLAY NC"翌月末日　＝" WK-SIME99 UPON CONS.
*T
*
*経理締日の１日前を求める
*T
     DISPLAY NC"経理〆日　＝" WK-SIME UPON CONS.
*T
     COMPUTE  WK-SIME  =  WK-SIME  -  1.
*T
     DISPLAY NC"締日１日前＝" WK-SIME UPON CONS.
*T
*#2016/11/14 NAV ST システム日付の月初日を算出
     MOVE    1               TO   WK-SYSDT-DD.
*
     DISPLAY NC"システム日付の月初日＝" WK-SYSDT-ST UPON CONS.
*#2016/11/14 NAV ED システム日付の月初日を算出
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    条件ファイル検索
****************************************************************
 JYO-READ-SEC               SECTION.
     MOVE     "JYO-READ-SEC" TO   S-NAME.
*
     READ     JYOKEN1
       INVALID
              MOVE "INV"     TO   JYO-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   JYO-INV-FLG
     END-READ.
 JYO-READ-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* 伝票ＥＸＣＥＬ読込　終了するまで繰り返す。
     PERFORM  READ-CSV-SEC.
*
     IF       CSV-ENDFLG  =   SPACE
              PERFORM         HENSYU-CSV-SEC
              GO          TO  MAIN-100
     END-IF.
*
     MOVE    "END"        TO  END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*               伝票ＥＸＣＥＬ　順読込
****************************************************************
 READ-CSV-SEC                SECTION.
     MOVE     "READ-CSV-SEC"      TO   S-NAME.
*
     READ     DCSVXXX   AT   END
              MOVE      "Y"       TO   CSV-ENDFLG
              GO                  TO   READ-CSV-EXIT
     END-READ.
*カウント（総件数）
     ADD      1                   TO   CSV-CNT.
*ＥＸＣＥＬ種別チェック
     IF   LINK-IN-EXCEL   NOT =    CSV-F01
          DISPLAY NC"＃＃指定したＥＸＣＥＬ種別と＃＃" UPON CONS
          DISPLAY NC"＃＃データのＥＸＣＥＬ種別が＃＃" UPON CONS
          DISPLAY NC"＃＃異なります！　　　　　　＃＃" UPON CONS
          DISPLAY NC"＃＃指定ＥＸＣＥＬ種別　＝"
                                        LINK-IN-EXCEL  UPON CONS
          DISPLAY NC"＃＃"                             UPON CONS
          DISPLAY NC"＃＃データＥＸＣＥＬ種別＝"
                                        CSV-F01        UPON CONS
          DISPLAY NC"＃＃"                             UPON CONS
          MOVE  4001     TO   PROGRAM-STATUS
          STOP  RUN
     END-IF.
*1件目の行＝0チェック
     IF   INIT-FLG            =    "1"
          IF      CSV-F05     =     0
                  MOVE  CSV-F05   TO   SAV-CSV-F05
          ELSE
                  DISPLAY NC"＃＃ＥＸＣＥＬデータ１件目の＃＃"
                                                       UPON CONS
                  DISPLAY NC"＃＃行番号が０以外です！　　＃＃"
                                                       UPON CONS
                  MOVE  4001      TO   PROGRAM-STATUS
                  STOP  RUN
          END-IF
*         MOVE    " "             TO   INIT-FLG
     END-IF.
*カウント（明細件数）
     IF       CSV-F05   NOT =          0
              ADD       1         TO   CNT-MEISAI
     END-IF.
*カウント（作業区分別件数）
     IF    CSV-F05 = 0
           EVALUATE CSV-F06
              WHEN  "40"
                     ADD   1    TO   CNT-URIAGE
              WHEN  "41"
                     ADD   1    TO   CNT-HENPIN
              WHEN  "42"
                     ADD   1    TO   CNT-NEBIKI
              WHEN  OTHER
                     ADD   1   TO   CNT-UNKNOWN
           END-EVALUATE
     END-IF.
     IF    CSV-F05 NOT = 0
           EVALUATE SAV-0-F06
              WHEN  "40"
                     ADD   1    TO   CNT-URIAGE
              WHEN  "41"
                     ADD   1    TO   CNT-HENPIN
              WHEN  "42"
                     ADD   1    TO   CNT-NEBIKI
              WHEN  OTHER
                     ADD   1   TO   CNT-UNKNOWN
           END-EVALUATE
     END-IF.
*行カウントリセット
     IF       CSV-F01     =    "1" OR "2"
        IF    CSV-F05     =     0
              MOVE    0         TO   WK-GYO-CNT
        END-IF
     END-IF.
     IF       CSV-F01     =    "3"
        IF    CSV-F07 NOT =     SAV-CSV-F07
              MOVE    0         TO   WK-GYO-CNT
              MOVE    CSV-F07   TO   SAV-CSV-F07
        END-IF
     END-IF.
*非0行への強制セット項目保管
     IF       CSV-F01     =    "1" OR "2"
        IF    CSV-F05     =     0
*             伝票区分
              MOVE    CSV-F06   TO   SAV-0-F06
*             店舗CD
              MOVE    CSV-F07   TO   SAV-0-F07
*             出荷場所
              MOVE    CSV-F08   TO   SAV-0-F08
*             伝発場所
              MOVE    CSV-F09   TO   SAV-0-F09
*             発注日
              MOVE    CSV-F10   TO   SAV-0-F10
*             納品日
              MOVE    CSV-F11   TO   SAV-0-F11
*             分類
              MOVE    CSV-F12   TO   SAV-0-F12
*             商区
              MOVE    CSV-F13   TO   SAV-0-F13
*             伝区
              MOVE    CSV-F14   TO   SAV-0-F14
        END-IF
     END-IF.
     IF       CSV-F01     =    "3"
        IF    CSV-F05     =     0
*             伝票区分
              MOVE    CSV-F06   TO   SAV-0-F06
*             出荷場所
              MOVE    CSV-F08   TO   SAV-0-F08
*             伝発場所
              MOVE    CSV-F09   TO   SAV-0-F09
*             発注日
              MOVE    CSV-F10   TO   SAV-0-F10
*             納品日
              MOVE    CSV-F11   TO   SAV-0-F11
*             分類
              MOVE    CSV-F12   TO   SAV-0-F12
*             商区
              MOVE    CSV-F13   TO   SAV-0-F13
*             伝区
              MOVE    CSV-F14   TO   SAV-0-F14
        END-IF
     END-IF.
*
 READ-CSV-EXIT.
     EXIT.
***************************************************************
*             伝票ＥＸＣＥＬ　チェック編集
***************************************************************
 HENSYU-CSV-SEC             SECTION.
*
     MOVE    "HENSYU-CSV-SEC"      TO   S-NAME.
*
     MOVE     SPACE                TO   WK-ERR-KBN.
     INITIALIZE                         WK-ERR-TBL.
*
*データ項目チェック
     PERFORM   DATA-CHK-SEC.
*
*伝票ＥＸＣＥＬ取込ファイル出力
     PERFORM   DEX-WRITE-SEC.
*
 HENSYU-CSV-EXIT.
     EXIT.
***************************************************************
*             データ項目チェック
***************************************************************
 DATA-CHK-SEC     SECTION.
*
     MOVE   "DATA-CHK-SEC"       TO   S-NAME.
*
*TEST
*TESTDISPLAY NC"連番＝" CSV-F00  UPON CONS.
*TEST
*
*------------------------------------------*
 DATA-CHK-01.
*【必須項目チェック】 エラー区分１
*------------------------------------------*
*  『連番』
*      EXCEL種別1：必須
*      EXCEL種別2：必須
*      EXCEL種別3：必須
     IF  ( CSV-F00 < 1         ) OR
         ( CSV-F00 NOT NUMERIC )
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
*T
*T       DISPLAY NC"　連番エラー　" CSV-F00  UPON CONS
*T
     END-IF.
*
*  『制御区分』
*      EXCEL種別1：必須"D"
*      EXCEL種別2：必須" "
*      EXCEL種別3：必須" "
     IF    CSV-F01 = "1"
           IF  CSV-F02 NOT = "D"
               MOVE   "Y"          TO        WK-ERR-KBN
               MOVE   "1"          TO        ERR-KBN(01)
*T
*T             DISPLAY NC"　制御区分エラー" CSV-F02  UPON CONS
*T
     END-IF.
     IF    CSV-F01 = "2"
           IF  CSV-F02 NOT = " "
               MOVE   "Y"          TO        WK-ERR-KBN
               MOVE   "1"          TO        ERR-KBN(01)
*T
*T             DISPLAY NC"　制御区分エラー" CSV-F02  UPON CONS
*T
     END-IF.
     IF    CSV-F01 = "3"
           IF  CSV-F02 NOT = " "
               MOVE   "Y"          TO        WK-ERR-KBN
               MOVE   "1"          TO        ERR-KBN(01)
*T
*T             DISPLAY NC"　制御区分エラー" CSV-F02  UPON CONS
*T
     END-IF.
*
*  『取引先コード』
*      EXCEL種別1：必須
*      EXCEL種別2：必須
*      EXCEL種別3：必須
     IF    CSV-F03 NOT NUMERIC
         MOVE   "Y"          TO        WK-ERR-KBN
         MOVE   "1"          TO        ERR-KBN(01)
*T
*T       DISPLAY NC"　取引先コードエラー　" CSV-F03  UPON CONS
*T
     END-IF.
*
*  『伝票番号』
*      EXCEL種別1：必須
*      EXCEL種別2：必須
*      EXCEL種別3：必須"1"←非固定とする2016/10/11
     IF    CSV-F01 = "1"
           IF  ( CSV-F04 < 1         ) OR
               ( CSV-F04 NOT NUMERIC )
               MOVE   "Y"          TO        WK-ERR-KBN
               MOVE   "1"          TO        ERR-KBN(01)
*T
*T             DISPLAY NC"　伝票番号エラー" CSV-F04  UPON CONS
*T
     END-IF.
     IF    CSV-F01 = "2"
           IF  ( CSV-F04 < 1         ) OR
               ( CSV-F04 NOT NUMERIC )
               MOVE   "Y"          TO        WK-ERR-KBN
               MOVE   "1"          TO        ERR-KBN(01)
*T
*T             DISPLAY NC"　伝票番号エラー" CSV-F04  UPON CONS
*T
     END-IF.
     IF    CSV-F01 = "3"
*↓2016/10/11
*          IF  ( CSV-F04 NOT = 1     ) OR
*              ( CSV-F04 NOT NUMERIC )
           IF  ( CSV-F04 < 1         ) OR
               ( CSV-F04 NOT NUMERIC )
*↑2016/10/11
               MOVE   "Y"          TO        WK-ERR-KBN
               MOVE   "1"          TO        ERR-KBN(01)
*T
*T             DISPLAY NC"　伝票番号エラー" CSV-F04  UPON CONS
*T
     END-IF.
*
*  『行番号』
*      EXCEL種別1：必須
*      EXCEL種別2：必須
*      EXCEL種別3：必須
     IF    CSV-F05 NOT NUMERIC
           MOVE   "Y"          TO        WK-ERR-KBN
           MOVE   "1"          TO        ERR-KBN(01)
*T
*T         DISPLAY NC"　行番号エラー" CSV-F05  UPON CONS
*T
     END-IF.
*
*  『伝票区分』
*    EXCEL種別1：必須(行＝0のみ）
*    EXCEL種別2：必須(行＝0のみ）
*    EXCEL種別3：必須(行＝0のみ）←行≠0も同様2016/10/11
     IF    CSV-F05 =   0
           IF    CSV-F06 = SPACE
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T               DISPLAY NC"　伝票区分エラー" CSV-F06  UPON CONS
*T
           END-IF
     END-IF.
*↓2016/10/11
     IF    CSV-F05 NOT =   0
        IF    CSV-F01 = "3"
           IF    CSV-F06 = SPACE
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T               DISPLAY NC"　伝票区分エラー" CSV-F06  UPON CONS
*T
           END-IF
        END-IF
     END-IF.
*↑2016/10/11
*
*  『店舗コード』
*      EXCEL種別1：必須(行＝0のみ）
*      EXCEL種別2：必須(行＝0のみ）
*      EXCEL種別3：必須
     IF    CSV-F01 = "1"
           IF  CSV-F05  =  0
               IF  ( CSV-F07  <  1   ) OR
                   ( CSV-F07 NOT NUMERIC )
                     MOVE   "Y"          TO        WK-ERR-KBN
                     MOVE   "1"          TO        ERR-KBN(01)
*T
*T                DISPLAY NC"　店舗ＣＤエラー" CSV-F07 UPON CONS
*T
               END-IF
           END-IF
     END-IF.
     IF    CSV-F01 = "2"
           IF  CSV-F05  =  0
               IF  ( CSV-F07  <  1       ) OR
                   ( CSV-F07 NOT NUMERIC )
                     MOVE   "Y"          TO        WK-ERR-KBN
                     MOVE   "1"          TO        ERR-KBN(01)
*T
*T                DISPLAY NC"　店舗ＣＤエラー" CSV-F07 UPON CONS
*T
               END-IF
           END-IF
     END-IF.
     IF    CSV-F01 = "3"
           IF  CSV-F05  =  0
               IF  ( CSV-F07 NOT = 0     ) OR
                   ( CSV-F07 NOT NUMERIC )
                     MOVE   "Y"          TO        WK-ERR-KBN
                     MOVE   "1"          TO        ERR-KBN(01)
*T
*T                DISPLAY NC"　店舗ＣＤエラー" CSV-F07 UPON CONS
*T
               END-IF
           END-IF
     END-IF.
*
*  『出荷場所』
*      EXCEL種別1：必須(行＝0のみ）
*      EXCEL種別2：必須(行＝0のみ）
*      EXCEL種別3：必須(行＝0のみ）
     IF    CSV-F05 =   0
           IF    CSV-F08 = SPACE
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T             DISPLAY NC"　出荷場所エラー" CSV-F08  UPON CONS
*T
           END-IF
     END-IF.
*
*  『伝発場所』
*      EXCEL種別1：必須(行＝0のみ）
*      EXCEL種別2：必須(行＝0のみ）
*      EXCEL種別3：必須(行＝0のみ）
     IF    CSV-F05 =   0
           IF    CSV-F09 = SPACE
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T             DISPLAY NC"　伝発場所エラー" CSV-F09  UPON CONS
*T
           END-IF
     END-IF.
*
*  『発注日』
*      EXCEL種別1：必須(行＝0のみ）
*      EXCEL種別2：必須(行＝0のみ）
*      EXCEL種別3：必須(行＝0のみ）
     IF    CSV-F05 =   0
           IF  ( CSV-F10 = ZERO      ) OR
               ( CSV-F10 NOT NUMERIC )
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T               DISPLAY NC"　発注日エラー" CSV-F10  UPON CONS
*T
           END-IF
     END-IF.
*
*  『納品日』
*      EXCEL種別1：必須(行＝0のみ）
*      EXCEL種別2：必須(行＝0のみ）
*      EXCEL種別3：必須(行＝0のみ）
     IF    CSV-F05 =   0
           IF  ( CSV-F11 = ZERO      ) OR
               ( CSV-F11 NOT NUMERIC )
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T               DISPLAY NC"　納品日エラー" CSV-F11  UPON CONS
*T
           END-IF
     END-IF.
*
*  『分類』
*      EXCEL種別1：任意
*      EXCEL種別2：任意
*      EXCEL種別3：任意
*
*  『商区』
*      EXCEL種別1：任意
*      EXCEL種別2：任意
*      EXCEL種別3：任意
*
*  『伝区』
*      EXCEL種別1：任意
*      EXCEL種別2：任意
*      EXCEL種別3：任意
*
*  『相手商品コード』
*      EXCEL種別1：必須(行≠0のみ）
*      EXCEL種別2：必須(行≠0のみ）
*      EXCEL種別3：必須(行≠0のみ）
     IF    CSV-F05   NOT = 0
           IF    CSV-F15 = SPACE
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T               DISPLAY NC"　相手商品エラー" CSV-F15  UPON CONS
*T
           END-IF
     END-IF.
*
*  『商品名１』
*      EXCEL種別1：必須(行≠0のみ）
*      EXCEL種別2：必須(行≠0のみ）
*      EXCEL種別3：必須(行≠0のみ）
     IF    CSV-F05   NOT = 0
           IF    CSV-F16 = SPACE
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T               DISPLAY NC"　商品名エラー" CSV-F16  UPON CONS
*T
           END-IF
     END-IF.
*
*  『商品名２』
*      EXCEL種別1：任意
*      EXCEL種別2：任意
*      EXCEL種別3：任意
*
*  『数量』
*      EXCEL種別1：必須(行≠0のみ）
*      EXCEL種別2：必須(行≠0のみ）
*      EXCEL種別3：必須(行≠0のみ）
     IF    CSV-F05   NOT = 0
           IF  ( CSV-F18 = ZERO      ) OR
               ( CSV-F18 NOT NUMERIC )
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T               DISPLAY NC"　数量エラー" CSV-F18  UPON CONS
*T
           END-IF
     END-IF.
*
*  『原価単価』
*      EXCEL種別1：必須(行≠0のみ）
*      EXCEL種別2：必須(行≠0のみ）
*      EXCEL種別3：必須(行≠0のみ）
     IF    CSV-F05   NOT = 0
           IF    CSV-F19 NOT NUMERIC
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T               DISPLAY NC"　原単エラー" CSV-F19  UPON CONS
*T
           END-IF
     END-IF.
*
*  『売価単価』
*      EXCEL種別1：必須(行≠0のみ）
*      EXCEL種別2：必須(行≠0のみ）
*      EXCEL種別3：必須(行≠0のみ）
     IF    CSV-F05   NOT = 0
           IF    CSV-F20 NOT NUMERIC
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(01)
*T
*T               DISPLAY NC"　売単エラー" CSV-F20  UPON CONS
*T
           END-IF
     END-IF.
*
*  『備考』
*      EXCEL種別1：任意
*      EXCEL種別2：任意
*      EXCEL種別3：任意
*
*------------------------------------------*
 DATA-CHK-02.
*【整合性チェック】 エラー区分２
*------------------------------------------*
*  行番号０連続
     IF       INIT-FLG  =   "1"
              MOVE   " "     TO   INIT-FLG
              GO             TO   DATA-CHK-03
     END-IF.
     IF       CSV-F05   =   0
         IF   CSV-F05   =   SAV-CSV-F05
              MOVE     "Y"   TO   WK-ERR-KBN
              MOVE     "1"   TO   ERR-KBN(02)
*T
*T            DISPLAY NC"　行番号０連続"    UPON CONS
*T
              PERFORM   ERR-KBN02-SEC
         END-IF
     END-IF.
*  同連番連続
     IF  CSV-F01   =   "1" OR "2"
       IF ( CSV-F00   NOT NUMERIC     ) AND
          ( SAV-CSV-F00   NOT NUMERIC )
         IF   CSV-F00   =   SAV-CSV-F00
              MOVE     "Y"   TO   WK-ERR-KBN
              MOVE     "1"   TO   ERR-KBN(02)
*T
*T            DISPLAY NC"　同連番連続"    UPON CONS
*T            DISPLAY   "CSV-F00    ="  CSV-F00      UPON CONS
*T            DISPLAY   "SAV-CSV-F00="  SAV-CSV-F00  UPON CONS
*T
              PERFORM   ERR-KBN02-SEC
         END-IF
       END-IF
     END-IF.
*  行番号減少時0以外
     IF       CSV-F01   =   "1" OR "2"
       IF     CSV-F05   <   SAV-CSV-F05
         IF   CSV-F05   NOT = 0
              MOVE     "Y"   TO   WK-ERR-KBN
              MOVE     "1"   TO   ERR-KBN(02)
*T            DISPLAY NC"　行減少０以外"    UPON CONS
         END-IF
       END-IF
     END-IF.
*
     MOVE     CSV-F00   TO       SAV-CSV-F00.
     MOVE     CSV-F05   TO       SAV-CSV-F05.
*
*------------------------------------------*
 DATA-CHK-03.
*【取引先Mチェック】 エラー区分３
*------------------------------------------*
*    EXCEL種別1：存在（全行）
*    EXCEL種別2：存在（全行）
*    EXCEL種別3：存在（全行）
     MOVE     CSV-F03        TO   TOK-F01.
     PERFORM  TOK-READ-SEC.
     IF       TOK-INV-FLG =  "INV"
              MOVE    "Y"    TO   WK-ERR-KBN
              MOVE    "1"    TO   ERR-KBN(03)
*T
*T            DISPLAY NC"　取引先マスタなし"  CSV-F03  UPON CONS
*T
     END-IF.
*
*------------------------------------------*
 DATA-CHK-04.
*【店舗Mチェック】 エラー区分４
*------------------------------------------*
*    EXCEL種別1：存在(行＝0のみ）
*    EXCEL種別2：存在(行＝0のみ）
*    EXCEL種別3：存在(行≠0のみ）
     IF    CSV-F01 = "1"
           IF  CSV-F05  =  0
               MOVE     CSV-F03        TO   TEN-F52
               MOVE     CSV-F07        TO   TEN-F011
               PERFORM  TEN-READ-SEC
               IF       TEN-INV-FLG =  "INV"
                        MOVE    "Y"    TO   WK-ERR-KBN
                        MOVE    "1"    TO   ERR-KBN(04)
*T
*T            DISPLAY NC"　店舗マスタなし"  CSV-F03 "-" CSV-F07
*T                                                 UPON CONS
*T
           END-IF
     END-IF.
     IF    CSV-F01 = "2"
           IF  CSV-F05  =  0
               MOVE     CSV-F03        TO   TEN-F52
               MOVE     CSV-F07        TO   TEN-F011
               PERFORM  TEN-READ-SEC
               IF       TEN-INV-FLG =  "INV"
                        MOVE    "Y"    TO   WK-ERR-KBN
                        MOVE    "1"    TO   ERR-KBN(04)
*T
*T            DISPLAY NC"　店舗マスタなし"  CSV-F03 "-" CSV-F07
*T                                                 UPON CONS
*T
           END-IF
     END-IF.
     IF    CSV-F01 = "3"
           IF  CSV-F05  NOT =  0
               MOVE     CSV-F03        TO   TEN-F52
               MOVE     CSV-F07        TO   TEN-F011
               PERFORM  TEN-READ-SEC
               IF       TEN-INV-FLG =  "INV"
                        MOVE    "Y"    TO   WK-ERR-KBN
                        MOVE    "1"    TO   ERR-KBN(04)
*T
*T            DISPLAY NC"　店舗マスタなし"  CSV-F03 "-" CSV-F07
*T                                                 UPON CONS
*T
           END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-05.
*【倉庫Mチェック】 エラー区分5
*------------------------------------------*
*    EXCEL種別1：存在(行＝0のみ）
*    EXCEL種別2：存在(行＝0のみ）
*    EXCEL種別3：存在(行＝0のみ）
*
*  _出荷場所
     IF    CSV-F05  =  0
           MOVE     CSV-F08        TO   SOK-F01
           PERFORM  SOK-READ-SEC
           IF       SOK-INV-FLG =  "INV"
                    MOVE    "Y"    TO   WK-ERR-KBN
                    MOVE    "1"    TO   ERR-KBN(05)
*T
*T            DISPLAY NC"　出荷場所なし"  CSV-F08  UPON CONS
*T
           END-IF
     END-IF.
*  _伝発場所
     IF    CSV-F05  =  0
           MOVE     CSV-F09        TO   SOK-F01
           PERFORM  SOK-READ-SEC
           IF       SOK-INV-FLG =  "INV"
                    MOVE    "Y"    TO   WK-ERR-KBN
                    MOVE    "1"    TO   ERR-KBN(05)
*T
*T            DISPLAY NC"　伝発場所なし"  CSV-F09  UPON CONS
*T
           END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-06.
*【商品変換TBLチェック】 エラー区分6
*------------------------------------------*
*    EXCEL種別1：存在(行≠0のみ）
*    EXCEL種別2：存在(行≠0のみ）
*    EXCEL種別3：存在(行≠0のみ）
     IF    CSV-F05  NOT =  0
           MOVE     CSV-F03        TO     TBL-F01
           MOVE     CSV-F15        TO     TBL-F02
           PERFORM  TBL-READ-SEC
           IF       TBL-INV-FLG =  "INV"
                    MOVE    "Y"    TO   WK-ERR-KBN
                    MOVE    "1"    TO   ERR-KBN(06)
*T
*T           DISPLAY NC"　変換ＴＢＬなし　"  CSV-F03 "-" CSV-F15
*T                                          UPON CONS
*T
           END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-07.
*【商品名称Mチェック】 エラー区分7
*------------------------------------------*
*    EXCEL種別1：存在(行≠0のみ）
*    EXCEL種別2：存在(行≠0のみ）
*    EXCEL種別3：存在(行≠0のみ）
     IF  ( CSV-F05  NOT =  0     ) AND
         ( TBL-INV-FLG  =  "   " )
           MOVE     TBL-F031       TO     MEI-F011
           MOVE     TBL-F0321      TO     MEI-F0121
           MOVE     TBL-F0322      TO     MEI-F0122
           MOVE     TBL-F0323      TO     MEI-F0123
           PERFORM  MEI-READ-SEC
           IF       MEI-INV-FLG =  "INV"
                    MOVE    "Y"    TO   WK-ERR-KBN
                    MOVE    "1"    TO   ERR-KBN(07)
*T
*T            DISPLAY NC"　商品名称マスタなし　"  TBL-F031 "-"
*T                                              TBL-F0321 "-"
*T                                              TBL-F0322 "-"
*T                                              TBL-F0323
*T                                              UPON CONS
*T
           END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-08.
*【行番号６行超チェック】 エラー区分8
*------------------------------------------*
*    EXCEL種別1：(行≠0のみ）
*    EXCEL種別2：(行≠0のみ）
*    EXCEL種別3：(行≠0のみ）
     IF  CSV-F05 NOT = 0
         ADD     1           TO        WK-GYO-CNT
         IF      WK-GYO-CNT  >         6
                 MOVE   "Y"  TO        WK-ERR-KBN
                 MOVE   "1"  TO        ERR-KBN(8)
*T
*T             DISPLAY NC"　行ＭＡＸエラー" WK-GYO-CNT UPON CONS
*T
         END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-09.
*【伝票区分チェック】 エラー区分9
*------------------------------------------*
*    EXCEL種別1：(行＝0のみ）
*    EXCEL種別2：(行＝0のみ）
*    EXCEL種別3：(行＝0のみ）
     IF  ( CSV-F01 = "1" )  AND
         ( CSV-F05 =  0  )
           IF  ( CSV-F06 NOT = "40" ) AND
               ( CSV-F06 NOT = "41" ) AND
               ( CSV-F06 NOT = "42" )
                 MOVE   "Y"  TO        WK-ERR-KBN
                 MOVE   "1"  TO        ERR-KBN(9)
*T
*T               DISPLAY NC"　伝票区分エラー" CSV-F06 UPON CONS
*T
           END-IF
     END-IF.
     IF  ( CSV-F01 = "2" )  AND
         ( CSV-F05 =  0  )
           IF  ( CSV-F06 NOT = "40" ) AND
               ( CSV-F06 NOT = "41" ) AND
               ( CSV-F06 NOT = "42" )
                 MOVE   "Y"  TO        WK-ERR-KBN
                 MOVE   "1"  TO        ERR-KBN(9)
*T
*T               DISPLAY NC"　伝票区分エラー" CSV-F06 UPON CONS
*T
           END-IF
     END-IF.
     IF  ( CSV-F01 = "3" )  AND
         ( CSV-F05 =  0  )
           IF  ( CSV-F06 NOT = "40" )
                 MOVE   "Y"  TO        WK-ERR-KBN
                 MOVE   "1"  TO        ERR-KBN(9)
*T
*T               DISPLAY NC"　伝票区分エラー" CSV-F06 UPON CONS
*T
           END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-10.
*【制御区分チェック】 エラー区分10
*------------------------------------------*
*    EXCEL種別1：（全行）
*    EXCEL種別2：（全行）
*    EXCEL種別3：（全行）
     IF    CSV-F01 = "1"
           IF    CSV-F02 NOT = "D"
                 MOVE   "Y"  TO        WK-ERR-KBN
                 MOVE   "1"  TO        ERR-KBN(10)
*T
*T               DISPLAY NC"　制御区分エラー" CSV-F02 UPON CONS
*T
           END-IF
     END-IF.
     IF    CSV-F01 = "2"
           IF    CSV-F02 NOT = " "
                 MOVE   "Y"  TO        WK-ERR-KBN
                 MOVE   "1"  TO        ERR-KBN(10)
*T
*T               DISPLAY NC"　制御区分エラー" CSV-F02 UPON CONS
*T
           END-IF
     END-IF.
     IF    CSV-F01 = "3"
           IF    CSV-F02 NOT = " "
                 MOVE   "Y"  TO        WK-ERR-KBN
                 MOVE   "1"  TO        ERR-KBN(10)
*T
*T               DISPLAY NC"　制御区分エラー" CSV-F02 UPON CONS
*T
           END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-11.
*【発注日チェック】 エラー区分11
*------------------------------------------*
*    EXCEL種別1：(行＝0のみ）
*    EXCEL種別2：(行＝0のみ）
*    EXCEL種別3：(行＝0のみ）
     IF    CSV-F05 NOT =  0
           GO                TO   DATA-CHK-12
     END-IF.
* _論理チェック
     MOVE    "2"               TO      LINK-IN-KBN.
     MOVE     ZERO             TO      LINK-IN-YMD6.
     MOVE     CSV-F10          TO      LINK-IN-YMD8.
     MOVE     ZERO             TO      LINK-OUT-RET.
     MOVE     ZERO             TO      LINK-OUT-YMD.
     CALL    "SKYDTCKB"        USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET     NOT =   ZERO
*T
*T            DISPLAY NC"　発注日論理エラー"  CSV-F10 UPON CONS
*T
              MOVE    "Y"      TO      WK-ERR-KBN
              MOVE    "1"      TO      ERR-KBN(11)
              GO               TO      DATA-CHK-12
     END-IF.
*
* _「経理月」の１日≦発注日≦システム日付の翌月末
     IF ( WK-SIME01 <= CSV-F10   ) AND
        ( CSV-F10   <= WK-SIME99 )
         CONTINUE
     ELSE
*T
*T       DISPLAY NC"　発注日範囲エラー"  CSV-F10 UPON CONS
*T
         MOVE    "Y"      TO      WK-ERR-KBN
         MOVE    "1"      TO      ERR-KBN(11)
         GO               TO      DATA-CHK-12
     END-IF.
*
* _〆日との関連チェック
*   発注日≦（〆日-1日）はＯＫ
*####IF  CSV-F10       <=  WK-SIME
*####    CONTINUE
*####ELSE
*####    MOVE   "Y"        TO   WK-ERR-KBN
*####    MOVE   "1"        TO   ERR-KBN(11)
*T
*T      DISPLAY NC"　発注日大小エラー" UPON CONS
*T
*###    GO                TO   DATA-CHK-12
*####END-IF.
*
*------------------------------------------*
 DATA-CHK-12.
*【納品日チェック】 エラー区分12
*------------------------------------------*
*    EXCEL種別1：(行＝0のみ）
*    EXCEL種別2：(行＝0のみ）
*    EXCEL種別3：(行＝0のみ）
     IF    CSV-F05 NOT =  0
           GO                TO   DATA-CHK-13
     END-IF.
* _論理チェック
     MOVE    "2"               TO      LINK-IN-KBN.
     MOVE     ZERO             TO      LINK-IN-YMD6.
     MOVE     CSV-F11          TO      LINK-IN-YMD8.
     MOVE     ZERO             TO      LINK-OUT-RET.
     MOVE     ZERO             TO      LINK-OUT-YMD.
     CALL    "SKYDTCKB"        USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET     NOT =   ZERO
*T
*T            DISPLAY NC"　納品日論理エラー"  CSV-F11 UPON CONS
*T
              MOVE    "Y"      TO      WK-ERR-KBN
              MOVE    "1"      TO      ERR-KBN(12)
              GO               TO      DATA-CHK-13
     END-IF.
*
* _「経理月」の１日≦納品日≦システム日付の翌月末
     IF ( WK-SIME01 <= CSV-F11   ) AND
        ( CSV-F11   <= WK-SIME99 )
         CONTINUE
     ELSE
*T
*T       DISPLAY NC"　納品日範囲エラー"  CSV-F11 UPON CONS
*T
         MOVE    "Y"      TO      WK-ERR-KBN
         MOVE    "1"      TO      ERR-KBN(12)
         GO               TO      DATA-CHK-13
     END-IF.
*
* _〆日との関連チェック
*   納品日≦（〆日-1日）はＯＫ
*####IF  CSV-F11       <=  WK-SIME
*####    CONTINUE
*####ELSE
*####    MOVE   "Y"        TO   WK-ERR-KBN
*####    MOVE   "1"        TO   ERR-KBN(12)
*T
*T      DISPLAY NC"　納品日大小エラー" UPON CONS
*T
*####   GO                TO   DATA-CHK-13
*####END-IF.
*##2016/11/14 NAV ST
***経理月の締年月日の前日までは経理2分の処理が可能
* _「システム日付」の１日≦納品日≦システム日付の翌月末
*T   DISPLAY "WK-SYSDT-ST = " WK-SYSDT-ST  UPON CONS.
*T   DISPLAY "WK-SIME     = " WK-SIME      UPON CONS.
*T   DISPLAY "CSV-F11     = " CSV-F11      UPON CONS.
*T   DISPLAY "WK-SIME99   = " WK-SIME99    UPON CONS.
     IF ( WK-SIME < WK-DATE8  )
         IF ( WK-SYSDT-ST <= CSV-F11 ) AND
            ( CSV-F11   <= WK-SIME99 )
              CONTINUE
         ELSE
*T
*T       DISPLAY NC"　経理月入力制限エラー"  CSV-F11 UPON CONS
*T
              MOVE    "Y"      TO      WK-ERR-KBN
              MOVE    "1"      TO      ERR-KBN(12)
              GO               TO      DATA-CHK-13
         END-IF
     END-IF.
*------------------------------------------*
 DATA-CHK-13.
*【原価単価チェック】 エラー区分13
*------------------------------------------*
*    EXCEL種別1：(行≠0のみ）
*    EXCEL種別2：(行≠0のみ）
*    EXCEL種別3：(行≠0のみ）
     IF    CSV-F05   NOT = 0
           IF    CSV-F19 < 1
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(13)
*T
*T               DISPLAY NC"　原単￥０" CSV-F19  UPON CONS
*T
           END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-14.
*【売価単価チェック】 エラー区分14
*　　※￥0単価OKとなったため必須チェックと同一
*------------------------------------------*
*    EXCEL種別1：(行≠0のみ）
*    EXCEL種別2：(行≠0のみ）
*    EXCEL種別3：(行≠0のみ）
     IF    CSV-F05   NOT = 0
           IF    CSV-F20 NOT NUMERIC
                 MOVE   "Y"          TO        WK-ERR-KBN
                 MOVE   "1"          TO        ERR-KBN(14)
*T
*T               DISPLAY NC"　売単エラー" CSV-F20  UPON CONS
*T
           END-IF
     END-IF.
*
*------------------------------------------*
 DATA-CHK-09.
*【カウント（エラー件数）】
*------------------------------------------*
     IF       WK-ERR-KBN  =   "Y"
              ADD    1        TO     CNT-ERR
*T
*T            DISPLAY NC"ＮＧ連番＝" CSV-F00 UPON CONS
*T
     ELSE
              ADD    1        TO     CNT-OK
*T
*T            DISPLAY NC"ＯＫ連番＝" CSV-F00 UPON CONS
*T
     END-IF.
*
 DATA-CHK-EXIT.
     EXIT.
*
****************************************************************
*       伝票ＥＸＣＥＬ取込ファイル出力
****************************************************************
 DEX-WRITE-SEC               SECTION.
*
     MOVE       "DEX-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE             TO        DEX-REC.
     INITIALIZE                           DEX-REC.
*取込日付 F01
     MOVE     SYS-DATE8         TO        DEX-F01.
*取込時刻 F02
     MOVE     WK-TIME-HM        TO        DEX-F02.
*取込担当者部門CD F03
     MOVE     LINK-IN-BUMON     TO        DEX-F03.
*取込担当者CD F04
     MOVE     LINK-IN-TANCD     TO        DEX-F04.
*EXCELデータ情報
*　連番 F201
     MOVE     CSV-F00           TO        DEX-F201.
*　EXCEL種別 F202
     MOVE     CSV-F01           TO        DEX-F202.
*　制御区分 F203
     MOVE     CSV-F02           TO        DEX-F203.
*　取引先CD F204
     MOVE     CSV-F03           TO        DEX-F204.
*　伝票番号 F205
     MOVE     CSV-F04           TO        DEX-F205.
*　行番号 F206
     MOVE     CSV-F05           TO        DEX-F206.
*　伝票区分 F207
     MOVE     SAV-0-F06         TO        DEX-F207.
*　店舗CD F208
     IF       CSV-F01  =  "1" OR "2"
              IF       CSV-F05  =  0
                       MOVE     CSV-F07   TO   DEX-F208
              ELSE
                       MOVE     SAV-0-F07 TO   DEX-F208
              END-IF
     END-IF.
     IF       CSV-F01  =  "3"
              MOVE     CSV-F07       TO   DEX-F208
     END-IF.
*　出荷場所 F209
     MOVE     SAV-0-F08         TO        DEX-F209.
*　伝発場所 F210
     MOVE     SAV-0-F09         TO        DEX-F210.
*　発注日 F211
     MOVE     SAV-0-F10         TO        DEX-F211.
*　納品日 F212
     MOVE     SAV-0-F11         TO        DEX-F212.
*　分類 F213
     MOVE     SAV-0-F12         TO        DEX-F213.
*　商区 F214
     MOVE     SAV-0-F13         TO        DEX-F214.
*　伝区 F215
     MOVE     SAV-0-F14         TO        DEX-F215.
*　相手商品CD F216
     IF       CSV-F05 NOT = 0
              MOVE   CSV-F15    TO        DEX-F216
     END-IF.
*　商品名１ F217
     MOVE     CSV-F16           TO        DEX-F217.
*　商品名２ F218
     MOVE     CSV-F17           TO        DEX-F218.
*　数量 F219
     IF       CSV-F05 NOT = 0
              MOVE   CSV-F18    TO        DEX-F219
     END-IF.
*　原価単価 F220
     IF       CSV-F05 NOT = 0
              MOVE   CSV-F19    TO        DEX-F220
     END-IF.
*　売価単価 F221
     IF       CSV-F05 NOT = 0
              MOVE   CSV-F20    TO        DEX-F221
     END-IF.
*　備考 F222
     IF       CSV-F05 NOT = 0
              MOVE   CSV-F21    TO        DEX-F222
*#2017/01/16 NAV ST
     ELSE
              IF  CSV-F01  NOT =  "3"
                  IF  CSV-F21(1:1)  =  "9"
                      MOVE  SPACE   TO        DEX-F222
                      MOVE  "9"     TO        DEX-F222(1:1)
                  END-IF
              END-IF
*#2017/01/16 NAV ED
     END-IF.
*　サカタ商品CD F223
*　サカタ品単１ F224
*　サカタ品単２ F225
*　サカタ品単３ F226
     IF       CSV-F05 NOT = 0
        IF    TBL-INV-FLG =  "   "
              MOVE   TBL-F031   TO        DEX-F223
              MOVE   TBL-F0321  TO        DEX-F224
              MOVE   TBL-F0322  TO        DEX-F225
              MOVE   TBL-F0323  TO        DEX-F226
        END-IF
     END-IF.
*　自社得意先CD F227
     IF       TOK-INV-FLG =  "   "
              MOVE   TOK-F52    TO        DEX-F227
     END-IF.
*エラー情報
*　エラー区分   F301～F317
     IF   WK-ERR-KBN    NOT =   SPACE
          MOVE   "1"            TO        DEX-F301
          MOVE    ERR-KBN(01)   TO        DEX-F302
          MOVE    ERR-KBN(02)   TO        DEX-F303
          MOVE    ERR-KBN(03)   TO        DEX-F304
          MOVE    ERR-KBN(04)   TO        DEX-F305
          MOVE    ERR-KBN(05)   TO        DEX-F306
          MOVE    ERR-KBN(06)   TO        DEX-F307
          MOVE    ERR-KBN(07)   TO        DEX-F308
          MOVE    ERR-KBN(08)   TO        DEX-F309
          MOVE    ERR-KBN(09)   TO        DEX-F310
          MOVE    ERR-KBN(10)   TO        DEX-F311
          MOVE    ERR-KBN(11)   TO        DEX-F312
          MOVE    ERR-KBN(12)   TO        DEX-F313
          MOVE    ERR-KBN(13)   TO        DEX-F314
          MOVE    ERR-KBN(14)   TO        DEX-F315
          MOVE    ERR-KBN(15)   TO        DEX-F316
          MOVE    ERR-KBN(16)   TO        DEX-F317
     END-IF.
*
     WRITE    DEX-REC.
     ADD      1                  TO   DEX-CNT.
*
 WRITE-DEX-EXIT.
     EXIT.
****************************************************************
*               エラー区分０２　更新　
****************************************************************
 ERR-KBN02-SEC               SECTION.
     MOVE     "ERR-KBN02-SEC"     TO   S-NAME.
*
 ERR-KBN02-01.
*
     MOVE     " "                 TO   DEX-F301.
     MOVE     SAV-CSV-F00         TO   DEX-F201.
     START    DEXXXXL1   KEY  IS  >=   DEX-F301
                                       DEX-F201
         INVALID
              DISPLAY
                NC"＃　伝票ＥＸＣＥＬ取込ＳＴＡＲＴエラー！　＃"
                                                       UPON CONS
              MOVE  4001     TO   PROGRAM-STATUS
              STOP  RUN
     END-START.
*
 ERR-KBN02-02.
*
     READ     DEXXXXL1   NEXT     AT   END
              DISPLAY
                NC"＃　伝票ＥＸＣＥＬ取込　ＲＥＡＤエラー！　＃"
              MOVE  4001     TO   PROGRAM-STATUS
              STOP  RUN
     END-READ.
*
 ERR-KBN02-03.
*
*エラー区分０２更新
*
     IF    (  DEX-F301   = " "         ) AND
           (  DEX-F201   = SAV-CSV-F00 )
              IF         DEX-F303  NOT = "1"
                         ADD       1     TO        CNT-ERR
                         SUBTRACT  1     FROM      CNT-OK
              END-IF
              MOVE      "1"        TO    DEX-F301  DEX-F303
              REWRITE    DEX-REC
              READ       DEXXXXL1
                   NEXT  AT   END
                         GO        TO    ERR-KBN02-04
                   NOT   AT   END
                         GO        TO    ERR-KBN02-03
              END-READ
     END-IF.
*
 ERR-KBN02-04.
*
     CLOSE         DEXXXXL1.
     OPEN     I-O  DEXXXXL1.
*
 ERR-KBN02-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ検索
****************************************************************
 MEI-READ-SEC             SECTION.
*
     MOVE     "MEI-READ-SEC"    TO    S-NAME.
*
     READ      MEIMS1
          INVALID
               MOVE   "INV"     TO    MEI-INV-FLG
          NOT INVALID
               MOVE   "   "     TO    MEI-INV-FLG
     END-READ.
*
 MEI-READ-EXIT.
     EXIT.
****************************************************************
*               倉庫マスタ検索
****************************************************************
 SOK-READ-SEC               SECTION.
*
     MOVE     "SOK-READ-SEC"      TO   S-NAME.
*
     READ      ZSOKMS1
       INVALID
               MOVE    "INV"      TO   SOK-INV-FLG
          NOT INVALID
               MOVE    "   "      TO   SOK-INV-FLG
     END-READ.
*
 SOKO-READ-EXIT.
     EXIT.
****************************************************************
*               商品変換ＴＢＬ検索
****************************************************************
 TBL-READ-SEC               SECTION.
*
     MOVE     "TBL-READ-SEC"      TO   S-NAME.
*
     READ      SHOTBL1
       INVALID
               MOVE    "INV"      TO   TBL-INV-FLG
          NOT INVALID
               MOVE    "   "      TO   TBL-INV-FLG
     END-READ.
*
 TBL-READ-EXIT.
     EXIT.
****************************************************************
*               取引先マスタ検索
****************************************************************
 TOK-READ-SEC               SECTION.
*
     MOVE     "TOK-READ-SEC"      TO   S-NAME.
*
     READ      TOKMS2
       INVALID
               MOVE    "INV"      TO   TOK-INV-FLG
          NOT INVALID
               MOVE    "   "      TO   TOK-INV-FLG
     END-READ.
*
 TOK-READ-EXIT.
     EXIT.
****************************************************************
*               店舗マスタ検索
****************************************************************
 TEN-READ-SEC               SECTION.
*
     MOVE     "TEN-READ-SEC"      TO   S-NAME.
*
     READ      TENMS1
       INVALID
               MOVE    "INV"      TO   TEN-INV-FLG
          NOT INVALID
               MOVE    "   "      TO   TEN-INV-FLG
     END-READ.
*
 TEN-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
*  伝票EXCEL読込件数
     MOVE      CSV-CNT      TO    MSG-OUT00.
*    内.伝票明細総数
*    MOVE      CNT-MEISAI   TO    MSG-OUT01.
*    内.売上伝票明細数
     MOVE      CNT-URIAGE   TO    MSG-OUT02.
*    内.返品伝票明細数
     MOVE      CNT-HENPIN   TO    MSG-OUT03.
*    内.値引伝票明細数
     MOVE      CNT-NEBIKI   TO    MSG-OUT04.
*  伝票EXCEL取込ファイル作成件数
     MOVE      DEX-CNT      TO    MSG-OUT05.
*    内.エラーデータ
     MOVE      CNT-ERR      TO    MSG-OUT06.
*    内.伝票区分不明
     MOVE      CNT-UNKNOWN  TO    MSG-OUT07.
*    内.ＯＫデータ
     MOVE      CNT-OK       TO    MSG-OUT08.
**
*  伝票EXCEL読込件数
     DISPLAY   MSG-OUT0-FIL1 MSG-OUT0-FIL2 MSG-OUT00
               MSG-OUT0-FIL3 MSG-OUT0-FIL4           UPON CONS.
*    内.伝票明細総数
*    DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
*              MSG-OUT1-FIL3 MSG-OUT1-FIL4           UPON CONS.
*    内.売上伝票明細数
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4           UPON CONS.
*    内.返品伝票明細数
     DISPLAY   MSG-OUT3-FIL1 MSG-OUT3-FIL2 MSG-OUT03
               MSG-OUT3-FIL3 MSG-OUT3-FIL4           UPON CONS.
*    内.値引伝票明細数
     DISPLAY   MSG-OUT4-FIL1 MSG-OUT4-FIL2 MSG-OUT04
               MSG-OUT4-FIL3 MSG-OUT4-FIL4           UPON CONS.
*    伝票EXCEL取込ファイル作成件数
     DISPLAY   MSG-OUT5-FIL1 MSG-OUT5-FIL2 MSG-OUT05
               MSG-OUT5-FIL3 MSG-OUT5-FIL4           UPON CONS.
*    内.エラーデータ
     DISPLAY   MSG-OUT6-FIL1 MSG-OUT6-FIL2 MSG-OUT06
               MSG-OUT6-FIL3 MSG-OUT6-FIL4           UPON CONS.
*    内.伝票区分不明
     DISPLAY   MSG-OUT7-FIL1 MSG-OUT7-FIL2 MSG-OUT07
               MSG-OUT7-FIL3 MSG-OUT7-FIL4           UPON CONS.
*    内.ＯＫデータ
     DISPLAY   MSG-OUT8-FIL1 MSG-OUT8-FIL2 MSG-OUT08
               MSG-OUT8-FIL3 MSG-OUT8-FIL4           UPON CONS.
*
     CLOSE     DCSVXXX
               DEXXXXL1
               JYOKEN1
               ZSOKMS1
               MEIMS1
               TOKMS2
               TENMS1.
*
*ＯＵＴパラメタセット
* データEXCEL種別
     MOVE      CSV-F01      TO   LINK-OUT-EXCEL.
* 取込件数
     MOVE      CSV-CNT      TO    LINK-OUT-CNT1.
* 売上伝票明細件数
     MOVE      CNT-URIAGE   TO    LINK-OUT-CNT2.
* 返品伝票明細件数
     MOVE      CNT-HENPIN   TO    LINK-OUT-CNT3.
* 値引伝票明細件数
     MOVE      CNT-NEBIKI   TO    LINK-OUT-CNT4.
* エラー件数
     MOVE      CNT-ERR      TO    LINK-OUT-CNT5.
* 取込日付
     MOVE      SYS-DATE8    TO    LINK-OUT-DATE.
* 取込時刻
     MOVE      WK-TIME-HM   TO    LINK-OUT-TIME.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
