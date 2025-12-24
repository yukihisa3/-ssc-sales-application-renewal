****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援システム　　　　　　*
*    モジュール名　　　　：　本発データ振分処理（チェック）　　*
*    作成日／作成者　　　：　2015/05/19 TAKAHASHI              *
*    処理内容　　　　　　：　本発ＥＸＣＥＬより取込んだ箱数、　*
*    　　　　　　　　　　　　数量確定Ｆを読み、マスタのチェック*
*                            を行い、取込箱、数量確定Ｆを作成。*
*    更新日／更新者　　　：　2020/07/07 TAKAHASHI              *
*    処理内容　　　　　　：　日付エラー追加　　　　　　　　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY3840B.
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
*数量確定ファイル
     SELECT      RCVSUKF     ASSIGN    TO       DA-01-RCVSUKAK
                             ORGANIZATION        SEQUENTIAL
                             ACCESS    MODE      SEQUENTIAL
                             FILE      STATUS   INS-ST.
*箱数確定ファイル
     SELECT      RCVHAKF     ASSIGN    TO       DA-01-RCVHAKOS
                             ORGANIZATION        SEQUENTIAL
                             ACCESS    MODE      SEQUENTIAL
                             FILE      STATUS   INH-ST.
*取込数量確定データ
     SELECT      TRKAKUF     ASSIGN    TO       DA-01-VI-TRKAKUL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                        RECORD    KEY  OTS-F01  OTS-F02  OTS-F03
                             OTS-F110  OTS-F111 OTS-F112 OTS-F113
                             OTS-F11H  OTS-F115 OTS-F116 OTS-F119
                             OTS-F11A  OTS-F11B
                             FILE      STATUS   OTS-ST.
*取込箱数確定データ
     SELECT      TRHAKOF     ASSIGN    TO       DA-01-VI-TRHAKOL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                        RECORD    KEY  OTH-F01  OTH-F02  OTH-F03
                             OTH-F121  OTH-F122 OTH-F123 OTH-F124
                             OTH-F12B  OTH-F126 OTH-F127 OTH-F128
                             FILE      STATUS   OTH-ST.
*テクノス取込管理ファイル
     SELECT      HONKANF     ASSIGN    TO       DA-01-VI-HONKANL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                        RECORD    KEY  OTK-F01  OTK-F02  OTK-F03
                             OTK-F04   OTK-F05  OTK-F06  OTK-F07
                             FILE      STATUS   OTK-ST.
*取引先マスタ
     SELECT      HTOKMS      ASSIGN    TO       DA-01-VI-TOKMS2
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TOK-F01
                             FILE      STATUS   TOK-ST.
*店舗マスタ
     SELECT      NFTENMS     ASSIGN    TO       DA-01-VI-NFTENMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      TEN-F01   TEN-F02
                             FILE      STATUS   TEN-ST.
*商品コード変換テーブル
     SELECT      HSHOTBL     ASSIGN    TO       DA-01-VI-SHOTBL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SHO-F01
                                                SHO-F02
                             FILE      STATUS   SHO-ST.
*ナフコ商品マスタ
     SELECT     NFSHOMS       ASSIGN    TO      DA-01-VI-NFSHOMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MEI-F01
                             FILE      STATUS   MEI-ST.
*作場マスタ
     SELECT     SAKUMS       ASSIGN    TO       DA-01-VI-SAKUBAL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SAK-F01
                             FILE      STATUS   SAK-ST.
*担当者マスタ
     SELECT     HTANMS       ASSIGN    TO       DA-01-VI-TANMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01  TAN-F02
                             FILE      STATUS   TAN-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*箱数確定ファイル
 FD  RCVHAKF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    68        RECORDS.
     COPY        RCVHAKOS    OF        XFDLIB
     JOINING     INH         AS        PREFIX.
*数量確定ファイル
 FD  RCVSUKF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    31        RECORDS.
     COPY        RCVSUKAK    OF        XFDLIB
     JOINING     INS         AS        PREFIX.
*取込数量確定データ
 FD  TRKAKUF.
     COPY        TRKAKUF     OF        XFDLIB
     JOINING     OTS         AS        PREFIX.
*取込箱数確定データ
 FD  TRHAKOF.
     COPY        TRHAKOF     OF        XFDLIB
     JOINING     OTH         AS        PREFIX.
*テクノス取込管理ファイル
 FD  HONKANF.
     COPY        HONKANF     OF        XFDLIB
     JOINING     OTK         AS        PREFIX.
*取引先マスタ
 FD  HTOKMS.
     COPY        HTOKMS      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
*店舗マスタ
 FD  NFTENMS.
     COPY     NFTENMS        OF        XFDLIB
     JOINING  TEN            AS        PREFIX.
*商品変換テーブル
 FD  HSHOTBL.
     COPY     HSHOTBL        OF        XFDLIB
     JOINING  SHO            AS        PREFIX.
*商品名称マスタ
 FD  NFSHOMS.
     COPY     NFSHOMS        OF        XFDLIB
     JOINING  MEI            AS        PREFIX.
*作場マスタ
 FD  SAKUMS.
     COPY     SAKUBAF        OF        XFDLIB
     JOINING  SAK            AS        PREFIX.
*担当者マスタ
 FD  HTANMS.
     COPY     HTANMS         OF        XFDLIB
     JOINING  TAN            AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  INS-ST              PIC  X(02)  VALUE  SPACE.
     03  INH-ST              PIC  X(02)  VALUE  SPACE.
     03  OTS-ST              PIC  X(02)  VALUE  SPACE.
     03  OTH-ST              PIC  X(02)  VALUE  SPACE.
     03  OTK-ST              PIC  X(02)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
     03  TEN-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  SAK-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  INH-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  INS-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  INS-CNT             PIC  9(07)  VALUE  ZERO.
     03  INH-CNT             PIC  9(07)  VALUE  ZERO.
     03  OTS-CNT             PIC  9(07)  VALUE  ZERO.
     03  OTH-CNT             PIC  9(07)  VALUE  ZERO.
     03  OTK-CNT             PIC  9(07)  VALUE  ZERO.
     03  OTH-UPD             PIC  9(07)  VALUE  ZERO.
**** 03  WK-ERR-AREA.
     03  WK-ERR-TBL.
         05  ERR-CNT         PIC  9(02)  OCCURS  15.
     03  WK-ERR-KBN          PIC  X(01).
     03  ERR-CDHED           PIC  9(02).
     03  ERR-CDHAKO          PIC  9(02).
     03  DEN-CNT             PIC  9(09)  VALUE  ZERO.
**
 01  TAN-INV-FLG             PIC  X(03).
 01  HAK-INV-FLG             PIC  X(01).
 01  NFTENMS-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  WK-DEN-TANCD            PIC  X(06).
 01  WK-TOKCD                PIC  9(08)  VALUE  137607.
 01  WK-TOKCDX               PIC  X(08)  VALUE "00137607".
 01  ERR-FLG                 PIC  X(01)  VALUE  SPACE.
 01  WK-SOKCD-LINK           PIC  X(02)  VALUE  SPACE.
*
 01  WK-OTK-KBN              PIC  X(03).
 01  WK-OTK-KEY.
   03  WK-OTK-F03            PIC  9(08).
   03  WK-OTK-F04            PIC  9(08).
   03  WK-OTK-F05            PIC  9(04).
   03  WK-OTK-F06            PIC  9(08).
   03  WK-OTK-F07            PIC  X(02).
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*#2020/07/07 NAV ST
 01  WK-DATE8-ED.
     03  WK-Y-ED             PIC  9(04)  VALUE  ZERO.
     03  WK-M-ED             PIC  9(02)  VALUE  ZERO.
     03  WK-D-ED             PIC  9(02)  VALUE  ZERO.
 01  WK-ST-DATE              PIC  9(08)  VALUE  ZERO.
 01  WK-ED-DATE              PIC  9(08)  VALUE  ZERO.
*#2020/07/07 NAV ED
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-KBN          PIC  9(01).
         05  LI-KETA         PIC  9(01).
         05  LI-START        PIC  9(09).
         05  LI-END          PIC  9(09).
         05  LI-DENNO        PIC  9(09).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
         05  LO-NEXT         PIC  9(09).
*
 01  FILE-ERR.
     03  INS-ERR            PIC  N(10)  VALUE
                   NC"入力数量確定Ｆ　異常".
     03  INH-ERR             PIC  N(10)  VALUE
                   NC"入力箱数確定Ｆ　異常".
     03  SUT-ERR             PIC  N(10)  VALUE
                   NC"数量確定Ｆ　異常".
     03  HAK-ERR             PIC  N(10)  VALUE
                   NC"箱数確定Ｆ　異常".
     03  OTS-ERR             PIC  N(10)  VALUE
                   NC"取込数量確定ＯＴ異常".
     03  OTH-ERR             PIC  N(10)  VALUE
                   NC"取込箱数確定ＯＴ異常".
     03  OTK-ERR             PIC  N(10)  VALUE
                   NC"本発取込管理　　異常".
     03  TOK-ERR             PIC  N(10)  VALUE
                   NC"取引先マスタ異常".
     03  TEN-ERR             PIC  N(10)  VALUE
                   NC"ナフコ店舗マスタ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品変換テーブル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"ナフコ商品Ｍ　異常".
     03  SAK-ERR             PIC  N(10)  VALUE
                   NC"作場マスタ異常".
     03  TAN-ERR             PIC  N(10)  VALUE
                   NC"担当者マスタ　　異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY3840B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3840B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 箱数確定ファイル読込件数 = ".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 数量確定ファイル読込件数 = ".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT3.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 取込箱数確定　　作成件数 = ".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT4.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 取込数量確定　　作成件数 = ".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT5.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 取込箱数確定　　更新件数 = ".
         05  MSG-OUT05       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT6.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 取込管理ファイル更新件数 = ".
         05  MSG-OUT06       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
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
 01  LINK-BUMON                  PIC  X(04).
 01  LINK-TANCD                  PIC  X(02).
 01  LINK-DATE                   PIC  9(08).
 01  LINK-TIME                   PIC  9(06).
 01  LINK-TOKCD                  PIC  9(08).
 01  LINK-SAKUBACD               PIC  X(02).
 01  LINK-ERR                    PIC  X(01).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-BUMON
                             LINK-TANCD  LINK-DATE  LINK-TIME
                             LINK-TOKCD  LINK-SAKUBACD  LINK-ERR.
****************************************************************
 DECLARATIVES.
 INS-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RCVSUKF.
     DISPLAY     INS-ERR    UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     INS-ST     UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 INH-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RCVHAKF.
     DISPLAY     INH-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     INH-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 OTS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE TRKAKUF.
     DISPLAY     OTS-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     OTS-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 OTH-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE TRHAKOF.
     DISPLAY     OTH-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     OTH-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 OTK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HONKANF.
     DISPLAY     OTK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     OTK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFTENMS.
     DISPLAY     TEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFSHOMS.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SAK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SAKUMS.
     DISPLAY     SAK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SAK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TAN-ST      UPON      CONS.
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
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     DISPLAY     MSG-START    UPON  CONS.
*
     OPEN        INPUT       RCVSUKF   RCVHAKF.
     OPEN        I-O         HONKANF.
     OPEN        INPUT       HTOKMS    NFTENMS   HSHOTBL  NFSHOMS
                             SAKUMS    HTANMS.
     OPEN        I-O         TRKAKUF   TRHAKOF.
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
*#2020/07/07 NAV ST
*****MOVE      LINK-OUT-YMD       TO   WK-DATE8.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8
                                       WK-ST-DATE
                                       WK-DATE8-ED.
     ADD       1                  TO   WK-M-ED.
     IF  WK-M-ED  >  12
         ADD   1                  TO   WK-Y-ED
         MOVE  1                   TO  WK-M-ED
     END-IF.
     MOVE      WK-DATE8-ED        TO   WK-ED-DATE.
     DISPLAY NC"日付範囲" " = " WK-ST-DATE " - " WK-ED-DATE
             UPON CONS.
*#2020/07/07 NAV ED
     DISPLAY "# HIDUKE = " WK-DATE8   UPON CONS.
     DISPLAY "# JIKAN  = " WK-TIME-HM UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* 箱数確定ファイル読込　終了するまで繰り返す。
     PERFORM  READ-INH-SEC.
*
     IF  INH-ENDFLG  =  SPACE
         PERFORM  HENSYU-INH-SEC
         GO  TO   MAIN-100
     END-IF.
*
 MAIN-200.
* 数量確定ファイル読込　終了するまで繰り返す。
     PERFORM  READ-INS-SEC.
*
     IF  INS-ENDFLG  =  SPACE
         PERFORM   HENSYU-INS-SEC
         GO  TO    MAIN-200
     END-IF.
*
     MOVE  "END"  TO   END-FLG.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*               箱数確定ファイル　順読込
****************************************************************
 READ-INH-SEC                SECTION.
     MOVE     "READ-INH-SEC"      TO   S-NAME.
*
     READ     RCVHAKF   AT   END
              MOVE      "Y"       TO   INH-ENDFLG
              GO        TO        READ-INH-EXIT
     END-READ.
*
     ADD      1   TO    INH-CNT.
*
 READ-INH-EXIT.
     EXIT.
****************************************************************
*               数量確定ファイル　順読込
****************************************************************
 READ-INS-SEC                SECTION.
     MOVE     "READ-INS-SEC"      TO   S-NAME.
*
     READ     RCVSUKF   AT   END
              MOVE      "Y"       TO   INS-ENDFLG
              GO        TO        READ-INS-EXIT
     END-READ.
*
     ADD      1   TO    INS-CNT.
*
 READ-INS-EXIT.
     EXIT.
***************************************************************
* 箱数確定ファイル　チェック編集
***************************************************************
 HENSYU-INH-SEC             SECTION.
* 箱数確定ファイル検索
     MOVE      SPACE    TO  WK-ERR-KBN.
     MOVE      ZERO     TO  ERR-CDHED.
     MOVE      ZERO     TO  ERR-CDHAKO.
     MOVE      SPACE    TO  HAK-INV-FLG.
* 取込箱数確定作成
     PERFORM  WRITE-OTH-SEC.
     MOVE      "INH"        TO    WK-OTK-KBN.
*管理番号＝０で更新する。
*****MOVE      INH-F01      TO    WK-OTK-F03.
     MOVE      ZERO         TO    WK-OTK-F03.
     MOVE      INH-F02      TO    WK-OTK-F04.
     MOVE      INH-F03      TO    WK-OTK-F05.
     MOVE      INH-F04      TO    WK-OTK-F06.
     MOVE      INH-F11      TO    WK-OTK-F07  WK-SOKCD-LINK.
* 取込管理ファイル更新
     PERFORM  OTK-UPD-SEC.
*
 HENSYU-INH-EXIT.
     EXIT.
****************************************************************
*              取込箱数確定データ　出力
****************************************************************
 WRITE-OTH-SEC               SECTION.
     MOVE     "WRITE-OTH-SEC" TO       S-NAME.
     MOVE        SPACE       TO        OTH-REC.
     INITIALIZE  OTH-REC.
**本発（手書分）のためバッチ日付、時刻にデフォルトセット
     MOVE     "2"            TO        OTH-F01.
     MOVE     WK-DATE8       TO        OTH-F02.
     MOVE     WK-TIME-HM     TO        OTH-F03.
     MOVE     LINK-TANCD     TO        OTH-F04.
     MOVE     LINK-BUMON     TO        OTH-F05.
*箱数確定Ｆが存在した場合
     IF  WK-ERR-KBN  NOT  =  SPACE
         MOVE   "1"          TO        OTH-F06
         MOVE    1           TO        OTH-F08
     END-IF.
*ＳＦ箱数確定ファイル内容を転送する
     MOVE     INH-REC(2:58)  TO        OTH-F12.
     MOVE     ZERO           TO        OTH-F121.
     MOVE     99999999       TO        OTH-F122.
     MOVE     9999           TO        OTH-F123.
     MOVE     WK-TOKCD       TO        OTH-F124.
*****ナフコ店舗マスタを索引する
     MOVE     WK-TOKCD       TO        TEN-F01
     MOVE     INH-F06        TO        TEN-F02
*##2015/09/09 NAV ST 店舗マスタ索引復活
*##  PERFORM  HTEN-HDRD-SEC.
*##  IF  NFTENMS-INV-FLG  =  SPACE
*##      MOVE TEN-F14        TO        OTH-F126
*##      MOVE TEN-F03        TO        OTH-F127
*##  END-IF.
*##2015/09/09 NAV ED 店舗マスタ索引復活
     WRITE    OTH-REC.
     ADD     1    TO   OTH-CNT.
*
 WRITE-OTH-EXIT.
     EXIT.
***************************************************************
* 数量確定ファイル　チェック編集
***************************************************************
 HENSYU-INS-SEC             SECTION.
*
     MOVE     SPACE    TO   WK-ERR-KBN.
     INITIALIZE        WK-ERR-TBL.
* マスタ存在チェック
     PERFORM   MAST-CHK-SEC.
*チェック位置変更
     PERFORM  OTH-UPD-SEC.
*2013/07/04 NAV ED 確定Ｆにエラーがなくて箱Ｆの存在チェックを
*　　　　　　　　　する様に変更する。
* 取込数量確定出力
     PERFORM  WRITE-OTS-SEC.
*
     MOVE      "INS"        TO    WK-OTK-KBN.
*管理番号＝０をセット
*****MOVE      INS-F01      TO    WK-OTK-F03.
     MOVE      ZERO         TO    WK-OTK-F03.
     MOVE      INS-F02      TO    WK-OTK-F04.
     MOVE      INS-F03      TO    WK-OTK-F05.
     MOVE      INS-F04      TO    WK-OTK-F06.
*****MOVE      INS-F05      TO    WK-OTK-F07.
     MOVE      INS-F18      TO    WK-OTK-F07   WK-SOKCD-LINK.
* 取込管理ファイル更新
     PERFORM  OTK-UPD-SEC.
*
 HENSYU-INS-EXIT.
     EXIT.
***************************************************************
*             マスタチェック
***************************************************************
 MAST-CHK-SEC     SECTION.
*
     PERFORM  NMEI-READ-SEC.
*
     PERFORM  HSHO-READ-SEC.
*
     PERFORM  HTOK-READ-SEC.
*
     PERFORM  HTEN-READ-SEC.
*
     PERFORM  SAKU-READ-SEC.
* 原価単価判定
     IF  INS-F20 = ZERO
     OR  INS-F20 NOT NUMERIC
         MOVE   "Y"          TO        WK-ERR-KBN  ERR-FLG
         MOVE    1           TO        ERR-CNT(06)
     END-IF.
* 売価単価判定
     IF  INS-F10 = ZERO
     OR  INS-F21 NOT NUMERIC
         MOVE   "Y"          TO        WK-ERR-KBN  ERR-FLG
         MOVE    1           TO        ERR-CNT(07)
     END-IF.
*#2020/07/07 NAV ST 日付範囲チェック検証
*    納品日
     IF  INS-F08  >=  WK-ST-DATE
     AND INS-F08  <=  WK-ED-DATE
         CONTINUE
     ELSE
         MOVE   "Y"          TO        WK-ERR-KBN  ERR-FLG
         MOVE    1           TO        ERR-CNT(08)
     END-IF.
*    出荷日
     IF  INS-F09  >=  WK-ST-DATE
     AND INS-F09  <=  WK-ED-DATE
         CONTINUE
     ELSE
         MOVE   "Y"          TO        WK-ERR-KBN  ERR-FLG
         MOVE    1           TO        ERR-CNT(09)
     END-IF.
*    入荷予定日
     IF  INS-F10  >=  WK-ST-DATE
     AND INS-F10  <=  WK-ED-DATE
         CONTINUE
     ELSE
         MOVE   "Y"          TO        WK-ERR-KBN  ERR-FLG
         MOVE    1           TO        ERR-CNT(10)
     END-IF.
*#2020/07/07 NAV ST 日付範囲チェック検証
 MAST-CHK-EXIT.
     EXIT.
*
****************************************************************
*              取込管理ファイル更新                           *
****************************************************************
 OTK-UPD-SEC              SECTION.
*
     MOVE      "OTK-UPD-SEC"   TO    S-NAME.
*
     MOVE      WK-DATE8     TO    OTK-F01.
     MOVE      WK-TIME-HM   TO    OTK-F02.
*管理番号には０をセットする。
*****MOVE      WK-OTK-F03   TO    OTK-F03.
     MOVE      ZERO         TO    OTK-F03.
     MOVE      WK-OTK-F04   TO    OTK-F04.
     MOVE      WK-OTK-F05   TO    OTK-F05.
     MOVE      WK-OTK-F06   TO    OTK-F06.
     MOVE      WK-OTK-F07   TO    OTK-F07   WK-SOKCD-LINK.
*
     READ      HONKANF
          INVALID
               GO  TO      OTK-UPD-100
     END-READ.
*
     IF  WK-OTK-KBN    =  "INS"
         ADD      1    TO   OTK-F08
     ELSE
         ADD      1    TO   OTK-F09
     END-IF.
     MOVE    LINK-TANCD    TO     OTK-F10.
     MOVE    LINK-BUMON    TO     OTK-F11.
*
     REWRITE  OTK-REC.
     ADD     1    TO   OTK-CNT.
     GO  TO       OTK-UPD-EXIT.
*
 OTK-UPD-100.
     MOVE      SPACE        TO    OTK-REC.
     INITIALIZE        OTK-REC.
     MOVE      WK-DATE8     TO    OTK-F01.
     MOVE      WK-TIME-HM   TO    OTK-F02.
*管理番号＝０で更新
*****MOVE      WK-OTK-F03   TO    OTK-F03.
     MOVE      ZERO         TO    OTK-F03.
     MOVE      WK-OTK-F04   TO    OTK-F04.
     MOVE      WK-OTK-F05   TO    OTK-F05.
     MOVE      WK-OTK-F06   TO    OTK-F06.
     MOVE      WK-OTK-F07   TO    OTK-F07  WK-SOKCD-LINK.
*
     IF  WK-OTK-KBN    =  "INS"
         MOVE     1    TO   OTK-F08
         MOVE     0    TO   OTK-F09
     ELSE
         MOVE     1    TO   OTK-F09
         MOVE     0    TO   OTK-F08
     END-IF.
     MOVE    LINK-TANCD    TO     OTK-F10.
     MOVE    LINK-BUMON    TO     OTK-F11.
*
     WRITE    OTK-REC.
     ADD     1    TO   OTK-CNT.
*
 OTK-UPD-EXIT.
     EXIT.
****************************************************************
*              取込箱数確定ファイル更新                       *
****************************************************************
 OTH-UPD-SEC              SECTION.
*
     MOVE      "OTH-UPD-SEC"   TO    S-NAME.
*
     MOVE      "2"          TO    OTH-F01.
     MOVE      WK-DATE8     TO    OTH-F02.
     MOVE      WK-TIME-HM   TO    OTH-F03.
*****MOVE      INS-F01      TO    OTH-F121.
     MOVE      ZERO         TO    OTH-F121.
     MOVE      INS-F02      TO    OTH-F122
     MOVE      INS-F03      TO    OTH-F123.
*****MOVE      INS-F04      TO    OTH-F124.
     MOVE      WK-TOKCD     TO    OTH-F124.
     MOVE      WK-TOKCDX    TO    LINK-TOKCD.
     MOVE      INS-F18      TO    OTH-F12B  LINK-SAKUBACD.
     MOVE      INS-F06      TO    OTH-F126.
     MOVE      INS-F07      TO    OTH-F127.
     MOVE      INS-F10      TO    OTH-F128.
*****ナフコ店舗マスタを索引する
     MOVE     WK-TOKCD       TO        TEN-F01
     MOVE     INS-F06        TO        TEN-F02
*##2015/09/09 NAV ST 店舗マスタ索引復活
*##  PERFORM  HTEN-HDRD-SEC.
*##  IF  NFTENMS-INV-FLG  =  SPACE
*##      MOVE TEN-F14        TO        OTH-F126
*##      MOVE TEN-F03        TO        OTH-F127
*##  END-IF.
*##2015/09/09 NAV ED 店舗マスタ索引復活
* 取込箱数確定ファイル読込
     READ      TRHAKOF
          INVALID
               CONTINUE
          NOT  INVALID
               GO           TO    OTH-UPD-200
     END-READ.
*箱数ファイルが存在しない場合、箱数ファイルを作成する。
 OTH-UPD-100.
     MOVE     "2"           TO    OTH-F01.
*
     MOVE      WK-DATE8     TO    OTH-F02.
     MOVE      WK-TIME-HM   TO    OTH-F03.
*****MOVE      INS-F01      TO    OTH-F121.
     MOVE      ZERO         TO    OTH-F121.
     MOVE      INS-F02      TO    OTH-F122
     MOVE      INS-F03      TO    OTH-F123.
*****MOVE      INS-F04      TO    OTH-F124.
     MOVE      WK-TOKCD     TO    OTH-F124.
     MOVE      WK-TOKCDX    TO    LINK-TOKCD.
     MOVE      INS-F18      TO    OTH-F12B  LINK-SAKUBACD.
     MOVE      INS-F06      TO    OTH-F126.
     MOVE      INS-F07      TO    OTH-F127.
     MOVE      INS-F10      TO    OTH-F128.
     MOVE     "1"           TO    OTH-F06.
     MOVE      1            TO    OTH-F07.
     MOVE     "HED-MEI-ERR" TO    OTH-F99.
     WRITE  OTH-REC.
*
     MOVE      "Y"          TO    WK-ERR-KBN   ERR-FLG.
     MOVE       1           TO    ERR-CNT(15).
     GO                     TO    OTH-UPD-EXIT.
*存在した場合で、エラー区分が１の場合は、エラーとする。
 OTH-UPD-200.
*  明細にエラーが存在する場合
     IF   WK-ERR-KBN NOT = SPACE
          MOVE      "1"          TO    OTH-F06
          REWRITE OTH-REC
     END-IF.
     IF   OTH-F06 NOT =  SPACE
          MOVE      "Y"          TO    WK-ERR-KBN  ERR-FLG
          MOVE       1           TO    ERR-CNT(15)
     END-IF.
*
 OTH-UPD-EXIT.
     EXIT.
****************************************************************
*              取込数量確定データ　出力
****************************************************************
 WRITE-OTS-SEC               SECTION.
     MOVE     "WRITE-OTS-SEC"    TO   S-NAME.
     MOVE        SPACE       TO   OTS-REC.
     INITIALIZE  OTS-REC.
* 手書き固定
     MOVE    "2"             TO        OTS-F01.
     MOVE     WK-DATE8       TO        OTS-F02.
     MOVE     WK-TIME-HM     TO        OTS-F03.
     MOVE     LINK-TANCD     TO        OTS-F04.
     MOVE     LINK-BUMON     TO        OTS-F05.
* 原価単価判定
     IF  INS-F20 = ZERO
     OR  INS-F20 NOT NUMERIC
         MOVE   "Y"          TO        WK-ERR-KBN  ERR-FLG
         MOVE    1           TO        ERR-CNT(06)
     END-IF.
* 売価単価判定
     IF  INS-F10 = ZERO
     OR  INS-F21 NOT NUMERIC
         MOVE   "Y"          TO        WK-ERR-KBN  ERR-FLG
         MOVE    1           TO        ERR-CNT(07)
     END-IF.
* エラー判定
     IF  WK-ERR-KBN  NOT  =  SPACE
         MOVE    "1"         TO        OTS-F06
         MOVE    WK-ERR-TBL  TO        OTS-F07
     END-IF.
*
*****MOVE     INS-F01        TO        OTS-F110.
     MOVE     ZERO           TO        OTS-F110.
     MOVE     INS-F02        TO        OTS-F111.
     MOVE     INS-F03        TO        OTS-F112.
*****MOVE     INS-F04        TO        OTS-F113.
     MOVE     WK-TOKCD       TO        OTS-F113.
     MOVE     INS-F05        TO        OTS-F114.
     MOVE     INS-F06        TO        OTS-F115.
     MOVE     INS-F07        TO        OTS-F116.
* 店舗ＣＤ／納品場所取得
     MOVE     WK-TOKCD       TO        TEN-F01.
     MOVE     WK-TOKCDX      TO        LINK-TOKCD.
     MOVE     INS-F06        TO        TEN-F02.
*##2015/09/09 NAV ST 店舗マスタ索引復活
*##  PERFORM  HTEN-HDRD-SEC.
*##  IF  NFTENMS-INV-FLG  =  SPACE
*##      MOVE TEN-F14        TO        OTS-F115
*##      MOVE TEN-F03        TO        OTS-F116
*##  END-IF.
*##2015/09/09 NAV ED 店舗マスタ索引復活
     MOVE     INS-F08        TO        OTS-F117.
     MOVE     INS-F09        TO        OTS-F118.
     MOVE     INS-F10        TO        OTS-F119.
     MOVE     INS-F11        TO        OTS-F11A.
     ADD      1              TO        DEN-CNT.
     MOVE     DEN-CNT        TO        OTS-F11A.
     MOVE     INS-F12        TO        OTS-F11B.
     MOVE     INS-F13        TO        OTS-F11C.
     MOVE     INS-F14        TO        OTS-F11D.
     MOVE     INS-F15        TO        OTS-F11E.
     MOVE     INS-F16        TO        OTS-F11F.
     MOVE     INS-F17        TO        OTS-F11G.
     MOVE     INS-F18        TO        OTS-F11H  LINK-SAKUBACD.
     MOVE     INS-F19        TO        OTS-F11I.
     MOVE     INS-F20        TO        OTS-F11J.
     MOVE     INS-F21        TO        OTS-F11K.
     MOVE     INS-F22        TO        OTS-F11L.
     MOVE     INS-F23        TO        OTS-F11M.
*
     WRITE    OTS-REC.
     ADD     1    TO   OTS-CNT.
*
 WRITE-OTS-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 NMEI-READ-SEC             SECTION.
*
     MOVE      "NMEI-READ-SEC"    TO    S-NAME.
*
     MOVE      INS-F13     TO     MEI-F01.
*
     READ      NFSHOMS
          INVALID
               MOVE   "Y"  TO     WK-ERR-KBN  ERR-FLG
               MOVE    1   TO     ERR-CNT(01)
     END-READ.
*
 NFSHOMS-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHO-READ-SEC             SECTION.
*
     MOVE      "HSHO-READ-SEC" TO    S-NAME.
*
     MOVE      WK-TOKCD    TO     SHO-F01.
     MOVE      INS-F13     TO     SHO-F02.
     READ      HSHOTBL
         INVALID
               MOVE   "Y"  TO     WK-ERR-KBN  ERR-FLG
               MOVE       1       TO    ERR-CNT(02)
     END-READ.
*
 HSHO-READ-EXIT.
     EXIT.
****************************************************************
*               取 引 先 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
 HTOK-READ-SEC               SECTION.
     MOVE     "HTOK-READ-SEC"     TO   S-NAME.
*
     MOVE     WK-TOKCD       TO   TOK-F01.
     READ     HTOKMS
       INVALID
               MOVE   "Y"  TO     WK-ERR-KBN  ERR-FLG
              MOVE       1        TO   ERR-CNT(03)
     END-READ.
 HTOK-READ-EXIT.
     EXIT.
****************************************************************
*               店舗　　 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
 HTEN-READ-SEC               SECTION.
     MOVE     "HTEN-READ-SEC"     TO   S-NAME.
*
     MOVE     WK-TOKCD       TO   TEN-F01.
     MOVE     INS-F06        TO   TEN-F02.
     READ     NFTENMS
       INVALID
               MOVE   "Y"  TO     WK-ERR-KBN  ERR-FLG
              MOVE       1        TO   ERR-CNT(04)
     END-READ.
 HTEN-READ-EXIT.
     EXIT.
****************************************************************
*    箱数確定Ｆ用の店舗マスタ読込み
****************************************************************
 HTEN-HDRD-SEC               SECTION.
     MOVE     "HTEN-HDRD-SEC"     TO   S-NAME.
*
     READ     NFTENMS
       INVALID
              MOVE "INV"     TO   NFTENMS-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   NFTENMS-INV-FLG
     END-READ.
 HTEN-HDRD-EXIT.
     EXIT.
****************************************************************
*               作場　　 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
 SAKU-READ-SEC               SECTION.
     MOVE     "SAKU-READ-SEC"     TO   S-NAME.
*
     MOVE     INS-F18        TO   SAK-F01.
     READ     SAKUMS
       INVALID
               MOVE   "Y"  TO     WK-ERR-KBN  ERR-FLG
              MOVE       1        TO   ERR-CNT(05)
     END-READ.
 SAKU-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE    WK-DATE8       TO   LINK-DATE.
     MOVE    WK-TIME-HM     TO   LINK-TIME.
*
     MOVE    INH-CNT        TO   MSG-OUT01.
     MOVE    INS-CNT        TO   MSG-OUT02.
     MOVE    OTH-CNT        TO   MSG-OUT03.
     MOVE    OTS-CNT        TO   MSG-OUT04.
     MOVE    OTH-UPD        TO   MSG-OUT05.
     MOVE    OTK-CNT        TO   MSG-OUT06.
     DISPLAY   MSG-OUT1     UPON  CONS.
     DISPLAY   MSG-OUT2     UPON  CONS.
     DISPLAY   MSG-OUT3     UPON  CONS.
     DISPLAY   MSG-OUT4     UPON  CONS.
     DISPLAY   MSG-OUT5     UPON  CONS.
     DISPLAY   MSG-OUT6     UPON  CONS.
*
     CLOSE       RCVSUKF  RCVHAKF                    TRKAKUF
                 TRHAKOF  HONKANF  HTOKMS   NFTENMS  HSHOTBL
                 NFSHOMS  SAKUMS   HTANMS.
*ＯＵＴパラメタセット
     IF  ERR-FLG  =  "Y"
         MOVE   "1"         TO    LINK-ERR
     ELSE
         MOVE   SPACE       TO    LINK-ERR
     END-IF.
*
     DISPLAY     MSG-END    UPON  CONS.
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TAN-READ-SEC              SECTION.
*
     INITIALIZE                          TAN-REC.
     MOVE       LINK-BUMON     TO        TAN-F01.
     MOVE       LINK-TANCD     TO        TAN-F02.
     READ       HTANMS
                INVALID
                MOVE    "INV"  TO        TAN-INV-FLG
                NOT INVALID
                MOVE    SPACE  TO        TAN-INV-FLG
     END-READ.
*
     IF  TAN-INV-FLG   =  SPACE
         IF   TAN-F06  = "1" OR "2" OR "3"
**************登録更新承認担当者・日付・代表権限・権限１
              MOVE    LINK-TANCD   TO    WK-DEN-TANCD
         ELSE
**************登録更新承認担当者・日付・代表権限・権限１
              MOVE    LINK-TANCD   TO    WK-DEN-TANCD
         END-IF
     ELSE
         DISPLAY NC"担当者Ｍ未登録" LINK-TANCD
                 UPON  CONS
     END-IF.
*
 TAN-READ-EXIT.
     EXIT.
