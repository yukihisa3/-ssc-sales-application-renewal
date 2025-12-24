****************************************************************
*    顧客名　　　　　　　：　サカタのタネ殿　　　　　　　　　  *
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　ナフコ発注確定処理（本発）        *
*    作成日／作成者　　　：　2015/05/01 NAV                    *
*    処理概要　　　　　　：　指定された管理番号について　　    *
*                            本発用ファイルからコピー　　　    *
*                            及び在庫更新を行なう。　　        *
*    更新日／更新者　　　：　2017/06/12 NAV                    *
*    処理概要　　　　　　：　商品名称マスタを索引し、小売連携  *
*                            区分をセットする。　　　　　　    *
*    更新日／更新者　　　：　2020/05/25 NAV                    *
*    更新概要　　　　　　：　Ｄ３６５連携対応　　　　　　　　  *
*    更新日／更新者　　　：　2020/12/21 NAV                    *
*    更新概要　　　　　　：　Ｄ３６５連携対応（小売連携区分）  *
*    　　　　　　　　　　　　Ｄ３６５連携番号採番（サブへ）　  *
*    更新日／更新者　　　：　2022/04/28 NAV                    *
*    更新概要　　　　　　：　本発時、受注時数量、単価、金額情報*
*    　　　　　　　　　　　　項目更新へ変更　　　　　　　　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY3832B.
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
*手書伝票基本データ
*    SELECT      SHTDENWK    ASSIGN    TO        DA-01-S-NFDENWK
*                            FILE      STATUS    DEN2-ST.
*本発基本情報ファイル
     SELECT   NHJOHOL1  ASSIGN    TO        DA-01-VI-NHJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       NHJ-F01   NHJ-F05
                                            NHJ-F06   NHJ-F07
                                            NHJ-F08   NHJ-F09
                        FILE      STATUS    NHJ-ST.
*本発売上伝票ファイル
     SELECT   NHTDENL1  ASSIGN    TO        DA-01-VI-NHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       NHT-F01   NHT-F02
                                            NHT-F04   NHT-F051
                                            NHT-F07   NHT-F112
                                            NHT-F03
                        FILE STATUS    IS   NHT-ST.
*基本情報ファイル
     SELECT   NFJOHOL1  ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       NFJ-F01   NFJ-F05
                                            NFJ-F06   NFJ-F07
                                            NFJ-F08   NFJ-F09
                        FILE      STATUS    NFJ-ST.
*売上伝票ファイル
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE STATUS    IS   DEN-ST.
*取引先マスタ
*    SELECT      HTOKMS      ASSIGN    TO        DA-01-VI-TOKMS2
*                            ORGANIZATION        INDEXED
*                            ACCESS    MODE      RANDOM
*                            RECORD    KEY       TOK-F01
*                            FILE      STATUS    TOK-ST.
*商品在庫マスタ
     SELECT     ZAMZAIL1     ASSIGN    TO        DA-01-VI-ZAMZAIL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                             FILE      STATUS    ZAI-ST.
*商品コード変換テーブル
     SELECT     SHOTBL1      ASSIGN    TO        DA-01-VI-SHOTBL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       SHO-F01
                                                 SHO-F02
                             FILE      STATUS    SHO-ST.
*商品名称マスタ
     SELECT     MEIMS1       ASSIGN    TO        DA-01-VI-MEIMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       MEI-F01
                             FILE      STATUS    MEI-ST.
*担当者マスタ
*    SELECT     HTANMS       ASSIGN    TO        DA-01-VI-TANMS1
*                            ORGANIZATION        INDEXED
*                            ACCESS    MODE      RANDOM
*                            RECORD    KEY       TAN-F01  TAN-F02
*                            FILE      STATUS    TAN-ST.
*2020/05/25
*---<<  条件ファイル  >>---*
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*#2020/12/21 NAV ST
*ＳＵＢ商品変換テーブル
     SELECT   SUBTBLL1  ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SUB-F01
                                            SUB-F02
                        FILE      STATUS    SUB-ST.
*#2020/12/21 NAV ED
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*伝票データＦ
*FD  SHTDENWK
*    LABEL       RECORD      IS        STANDARD
*    BLOCK       CONTAINS    1         RECORDS.
*    COPY        NFDENWK     OF        XFDLIB
*    JOINING     DEN2        AS        PREFIX.
*本発基本情報ファイル
 FD  NHJOHOL1
     LABEL       RECORD      IS        STANDARD.
     COPY        NHJOHOL1     OF        XFDLIB
     JOINING     NHJ         AS        PREFIX.
*本発売上伝票ファイル
 FD  NHTDENL1
     LABEL       RECORD      IS        STANDARD.
     COPY        NHTDENL1     OF        XFDLIB
     JOINING     NHT         AS        PREFIX.
*基本情報ファイル
 FD  NFJOHOL1
     LABEL       RECORD      IS        STANDARD.
     COPY        NFJOHOL1     OF        XFDLIB
     JOINING     NFJ         AS        PREFIX.
*売上伝票ファイル
 FD  SHTDENL1
     LABEL       RECORD      IS        STANDARD.
     COPY        SHTDENL1    OF        XFDLIB
     JOINING     DEN         AS        PREFIX.
*取引先マスタ
*FD  HTOKMS
*    LABEL       RECORD      IS        STANDARD
*    BLOCK       CONTAINS     8        RECORDS.
*    COPY        HTOKMS      OF        XFDLIB
*    JOINING     TOK         AS        PREFIX.
*商品在庫マスタ
 FD  ZAMZAIL1.
     COPY        ZAMZAIL1    OF        XFDLIB
                 JOINING     ZAI       PREFIX.
*商品変換テーブル
 FD  SHOTBL1.
     COPY        SHOTBL1     OF        XFDLIB
                 JOINING     SHO       PREFIX.
*商品名称マスタ
 FD  MEIMS1.
     COPY        MEIMS1      OF        XFDLIB
                 JOINING     MEI       PREFIX.
*担当者マスタ
*FD  HTANMS.
*    COPY        HTANMS      OF        XFDLIB
*                JOINING     TAN       PREFIX.
*2020/05/25
*---<<  条件ファイル  >>---*
 FD  JYOKEN1.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
*#2020/12/21 NAV ST
*商品変換テーブル
 FD  SUBTBLL1.
     COPY        SUBTBLL1     OF        XFDLIB
                 JOINING     SUB       PREFIX.
*#2020/12/21 NAV ED
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  NFJ-ST              PIC  X(02)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
     03  NHJ-ST              PIC  X(02)  VALUE  SPACE.
     03  NHT-ST              PIC  X(02)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
*2020/05/25
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
*2020/05/25
*2020/12/21
     03  SUB-ST              PIC  X(02)  VALUE  SPACE.
*2020/12/21
 01  WK-D365-DEN-PARA.
     03  WK-D365-PARA-IN1   PIC   X(01).
     03  WK-D365-PARA-IN2   PIC   9(08).
     03  WK-D365-PARA-OUT1  PIC   X(20).
     03  WK-D365-PARA-OUT2  PIC   X(01).
*    Ｄ３６５伝票番号採番値エリア
     COPY   JYOKEN1  OF XFDLIB  JOINING   JD3  AS   PREFIX.
*2020/05/25
*D365伝票番号編集
 01  WK-RENBAN               PIC  9(08).
 01  WK-D365-DEN.
     03  WK-D365-TYPE        PIC  X(01).
     03  WK-D365-TOKCD       PIC  9(08).
     03  WK-D365-DATE        PIC  9(06).
     03  WK-D365-RENBAN      PIC  9(05).
*
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  END-FLG1            PIC  X(03)  VALUE  SPACE.
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  INV-SW              PIC  9(01)  VALUE  ZERO.
     03  WK-DENCNT           PIC  9(09)  VALUE  ZERO.
     03  WK-NFJCNT           PIC  9(09)  VALUE  ZERO.
     03  READ-CNT1           PIC  9(09)  VALUE  ZERO.
     03  IX                  PIC  9(01)  VALUE  ZERO.
     03  WK-TOKCD            PIC  9(08)  VALUE  ZERO.
     03  WK-TENCD            PIC  9(05)  VALUE  ZERO.
     03  WK-DENNO            PIC  9(09)  VALUE  ZERO.
     03  WK-SYUKA            PIC  9(08)  VALUE  ZERO.
     03  WK-BASYO            PIC  X(02)  VALUE  SPACE.
     03  WK-TOKNM            PIC  N(10).
     03  L-CNT               PIC  99     VALUE  ZERO.
     03  P-CNT               PIC  99     VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  SHOTBL1-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  NFJOHOL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  NHTDENL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  SHTDENL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  MEIMS1-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  BIKO-INV-FLG        PIC  X(03)  VALUE  SPACE.
     03  WK-DENBIKO          PIC  9(09)  VALUE  ZERO.
     03  WK-DENPYO           PIC  9(09)  VALUE  ZERO.
     03  WK-TORBIKO          PIC  9(08)  VALUE  ZERO.
     03  TAN-INV-FLG         PIC  X(03)  VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)  VALUE  SPACE.
*2020/05/25
     03  JYOKEN1-INV-FLG     PIC  X(03)  VALUE  SPACE.
*#2020/12/21 NAV ST
     03  SUBTBLL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
*#2020/12/21 NAV ED
*
*計算領域
 01  WRK-AREA.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  DENNO-S                 PIC  9(09)  VALUE  999999999.
 01  DENNO-L                 PIC  9(09)  VALUE  ZERO.
 01  WK-DENBK                PIC  9(09)  VALUE  ZERO.
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
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
     03  JOH-ERR             PIC  N(10)  VALUE
                   NC"基本情報Ｆ　　異常".
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"伝票データＦ異常".
     03  NHJ-ERR             PIC  N(10)  VALUE
                   NC"本発基本情報Ｆ異常".
     03  NHT-ERR             PIC  N(10)  VALUE
                   NC"本発売上Ｆ　　異常".
     03  TOK-ERR             PIC  N(10)  VALUE
                   NC"取引先マスタ異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
     03  SHO-ERR             PIC  N(10)  VALUE
                   NC"商品変換テーブル異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  TAN-ERR             PIC  N(10)  VALUE
                   NC"担当者マスタ　　異常".
*2020/05/25
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル　　異常".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 01  WK-TOUSEI.
     03  WK-DEN-F59          PIC  X(02)  VALUE  SPACE.
     03  WK-DEN-F60          PIC  X(02)  VALUE  SPACE.
     03  WK-DEN-F61          PIC  X(02)  VALUE  SPACE.
     03  WK-DEN-F62          PIC  9(08)  VALUE  ZERO.
     03  WK-DEN-F63          PIC  9(08)  VALUE  ZERO.
     03  WK-DEN-F64          PIC  9(08)  VALUE  ZERO.
     03  WK-DEN-F65          PIC  X(01)  VALUE  SPACE.
     03  WK-DEN-F66          PIC  X(01)  VALUE  SPACE.
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
*01  LINK-WKSTNNM                PIC  X(08).
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-IN-KANRINO             PIC  9(08).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-IN-KANRINO.
****************************************************************
 DECLARATIVES.
*DEN2-ERR                    SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE SHTDENWK.
*    DISPLAY     DEN2-ERR    UPON      CONS.
*    DISPLAY     SEC-NAME    UPON      CONS.
*    DISPLAY     DEN2-ST     UPON      CONS.
*    MOVE        4000        TO        PROGRAM-STATUS.
*    ACCEPT      IN-DATA     FROM      CONS.
*    STOP        RUN.
 JOH-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NFJOHOL1.
     DISPLAY     JOH-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     NFJ-ST      UPON      CONS.
     DISPLAY "NFJ-F01 = " NFJ-F01  UPON CONS.
     DISPLAY "NFJ-F05 = " NFJ-F05  UPON CONS.
     DISPLAY "NFJ-F06 = " NFJ-F06  UPON CONS.
     DISPLAY "NFJ-F07 = " NFJ-F07  UPON CONS.
     DISPLAY "NFJ-F08 = " NFJ-F08  UPON CONS.
     DISPLAY "NFJ-F09 = " NFJ-F09  UPON CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENL1.
     DISPLAY     DEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 NHJ-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NHJOHOL1.
     DISPLAY     NHJ-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     NHJ-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 NHT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NHTDENL1.
     DISPLAY     NHT-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
*TOK-ERR                     SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
*    DISPLAY     TOK-ERR     UPON      CONS.
*    DISPLAY     SEC-NAME    UPON      CONS.
*    DISPLAY     TOK-ST      UPON      CONS.
*    MOVE        4000        TO        PROGRAM-STATUS.
*    ACCEPT      IN-DATA     FROM      CONS.
*    STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIL1.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHOTBL1.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
*TAN-ERR                     SECTION.
*    USE         AFTER       EXCEPTION PROCEDURE HTANMS.
*    DISPLAY     TAN-ERR     UPON      CONS.
*    DISPLAY     SEC-NAME    UPON      CONS.
*    DISPLAY     TAN-ST      UPON      CONS.
*    MOVE        4000        TO        PROGRAM-STATUS.
*    ACCEPT      IN-DATA     FROM      CONS.
*    STOP        RUN.
*2020/05/25
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
*    OPEN        I-O         SHTDENWK  HTOKMS  ZAMZAIF
*                INPUT       SHOTBL1   MEIMS1  HTANMS
*                I-O         SHTDENL1.
     OPEN        I-O         NFJOHOL1   SHTDENL1   ZAMZAIL1.
     OPEN        INPUT       NHJOHOL1 NHTDENL1 SHOTBL1 MEIMS1.
*#2020/12/21 NAV ST
     OPEN        INPUT       SUBTBLL1.
*#2020/12/21 NAV ED
*システム日付・時刻の取得
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
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
*パラ担当者で承認権限取得（内部統制更新項目）
*    PERFORM   TAN-READ-SEC.
*量販ＲＥＣ初期化
*    MOVE        SPACE       TO   DEN-REC.
*    INITIALIZE  DEN-REC.
 INIT-010.
*本発基本情報Ｆ　初期読み
*    PERFORM  DEN2-RD-SEC.
     PERFORM  NHJOHOL1-START-SEC.
     IF  END-FLG1 = "END"
         DISPLAY NC"＃本発基本　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         MOVE    9           TO   END-FLG
         GO                  TO   INIT-EXIT
     END-IF.
*
     PERFORM  NHJOHOL1-READ-SEC.
     IF  END-FLG1 = "END"
         DISPLAY NC"＃本発基本　データなし＃" UPON CONS
         MOVE    4010        TO   PROGRAM-STATUS
         MOVE    9           TO   END-FLG
         GO                  TO   INIT-EXIT
     END-IF.
*2020/05/25
     OPEN     INPUT  JYOKEN1.
     PERFORM  JYOKEN1-READ-SEC.
     IF  JYOKEN1-INV-FLG = SPACE
         MOVE  JYO-REC          TO      JD3-REC
     ELSE
         DISPLAY NC"条件ファイルなし" "KEY1=60,KEY2=D365NONF"
                                                 UPON CONS
         MOVE  4000             TO      PROGRAM-STATUS
         MOVE    9              TO      END-FLG
         GO                     TO      INIT-EXIT
     END-IF.
     CLOSE        JYOKEN1.
     DISPLAY "## D365" NC"取得伝票"  "NO = " JD3-F04
             " ##" UPON CONS.
*
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　本発基本情報ファイルスタート
*--------------------------------------------------------------*
 NHJOHOL1-START-SEC   SECTION.
*
*    本発基本情報ファイルスタート
     MOVE        SPACE         TO  NHJ-REC.
     INITIALIZE                    NHJ-REC.
*
     MOVE   LINK-IN-KANRINO    TO  NHJ-F01.
     START  NHJOHOL1  KEY  IS  >=  NHJ-F01  NHJ-F05  NHJ-F06
                                   NHJ-F07  NHJ-F08  NHJ-F09
            INVALID
            MOVE   "END"        TO   END-FLG1
     END-START.
*
 NHJOHOL1-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　本発基本情報ファイル読込
*--------------------------------------------------------------*
 NHJOHOL1-READ-SEC    SECTION.
*
     READ     NHJOHOL1
              NEXT  AT  END
                  MOVE     "END"    TO  END-FLG1
                  GO                TO  NHJOHOL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT1
     END-READ.
*
     IF  READ-CNT1(7:3) = "000" OR "500"
         DISPLAY "READ-CNT1 = " READ-CNT1 UPON CONS
     END-IF.
*
     IF  LINK-IN-KANRINO < NHJ-F01
         MOVE     "END"    TO  END-FLG1
         GO                TO  NHJOHOL1-READ-EXIT
     END-IF.
*
 NHJOHOL1-READ-EXIT.
     EXIT.
****************************************************************
*          条件ファイル読込                                    *
****************************************************************
 JYOKEN1-READ-SEC          SECTION.
*
     MOVE      "JYOKEN1-READ-SEC" TO    S-NAME.
*
     MOVE      60          TO     JYO-F01.
     MOVE      "D365NONF"  TO     JYO-F02.
     READ      JYOKEN1
               INVALID
               MOVE      "INV"    TO    JYOKEN1-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-01.
*
*基本情報ファイル検索
     PERFORM  NFJOHOL1-READ-SEC.
     IF       NFJOHOL1-INV-FLG    =   "INV"
              MOVE     NHJ-REC    TO   NFJ-REC
     ELSE
              GO       TO         MAIN-99
     END-IF.
*
 MAIN-02.
*
*本発売上伝票ファイル検索
     PERFORM  NHTDENL1-READ-SEC.
     IF       NHTDENL1-INV-FLG    =   "INV"
              DISPLAY NC"＃本発売上　データなし＃" UPON CONS
              MOVE    4010        TO   PROGRAM-STATUS
              MOVE    9           TO   END-FLG
              GO                  TO   MAIN-EXIT
     END-IF.
*
 MAIN-03.
*
*売上伝票ファイル検索
     PERFORM  SHTDENL1-READ-SEC.
     IF       SHTDENL1-INV-FLG    =   "INV"
              MOVE     NHT-REC    TO   DEN-REC
**************2022/04/28 NAV ST 受注時項目をセット
              MOVE     DEN-F15    TO   DEN-F50
              MOVE     DEN-F172   TO   DEN-F512
              MOVE     DEN-F173   TO   DEN-F513
              MOVE     DEN-F181   TO   DEN-F521
              MOVE     DEN-F182   TO   DEN-F522
**************2022/04/28 NAV ED 受注時項目をセット
     ELSE
              GO       TO         MAIN-99
     END-IF.
*
*伝票データ作成
*****売上伝票ファイルを検索する
*    PERFORM  SHTDENL1-READ-SEC.
*既に登録済の場合は更新しない。
*    IF  SHTDENL1-INV-FLG = SPACE
*        GO                  TO        DEN2-READ
*    END-IF.
*
*    MOVE     DEN2-REC       TO        DEN-REC.
*    MOVE     DEN2-F02       TO        DEN-F23.
*    MOVE     ZERO           TO        DEN-F113.
*MAIN061.
*****内部統制対応
*    MOVE     WK-DEN-F59     TO        DEN-F59.
*    MOVE     WK-DEN-F60     TO        DEN-F60.
*    MOVE     WK-DEN-F61     TO        DEN-F61.
*    MOVE     WK-DEN-F62     TO        DEN-F62.
*    MOVE     WK-DEN-F63     TO        DEN-F63.
*    MOVE     WK-DEN-F64     TO        DEN-F64.
*    MOVE     WK-DEN-F65     TO        DEN-F65.
*    MOVE     WK-DEN-F66     TO        DEN-F66.
*    MOVE     ZERO           TO        DEN-F67.
*    MOVE     LINK-IN-TANCD     TO        DEN-F06.
*
 MAIN062.
*商品変換ＴＢＬチェック*
     PERFORM     SHOTBL1-READ-SEC.
*#2020/12/21 NAV ST
*TT  IF  SHOTBL1-INV-FLG  =  SPACE
*TT      *商品名称マスタチェック*
*TT      MOVE    SHO-F031  TO        MEI-F011
*TT      MOVE    SHO-F032  TO        MEI-F012
*TT      PERFORM  MEIMS1-READ-SEC
*TT******2017/06/12 NAV ST 小売連携区分をセットする。
*TT      IF  MEIMS1-INV-FLG = SPACE
*TT          MOVE MEI-F10  TO        DEN-F32
*TT      ELSE
*TT          MOVE SPACE    TO        DEN-F32
*TT      END-IF
*********2017/06/12 NAV ED
*#2020/12/21 NAV ST
     PERFORM     SUBTBLL1-READ-SEC.
*#2020/12/21 NAV ED
*****DISPLAY "SUBTBLL1-INV-FLG = " SUBTBLL1-INV-FLG UPON CONS.
*****DISPLAY "SUB-F01          = " SUB-F01          UPON CONS.
*****DISPLAY "SUB-F02          = " SUB-F02          UPON CONS.
     IF  SUBTBLL1-INV-FLG  =  SPACE
         MOVE  SUB-F19     TO        DEN-F32
     ELSE
         MOVE  SPACE       TO        DEN-F32
     END-IF.
*****DISPLAY "SUB-F19          = " SUB-F19          UPON CONS.
*****DISPLAY "DEN-F32          = " DEN-F32          UPON CONS.
*#2020/12/21 NAV ED
*TT  END-IF.
*仕入単価取得
*    IF     SHO-F09   =  ZERO
*        MOVE    MEI-F041  TO        DEN-F171
*    ELSE
*        MOVE    SHO-F09   TO        DEN-F171
*    END-IF.
 MAIN-04.
*在庫引当
     IF  SHOTBL1-INV-FLG  =  SPACE
         PERFORM  ZAIKO-SEC
         IF  KEP-FLG  =  SPACE
             MOVE     1       TO     DEN-F27D
         END-IF
         MOVE         1       TO     DEN-F262
     END-IF.
*2020/25/25
*Ｄ３６５伝票番号採番
*#2020/12/21 NAV ST
*TT  PERFORM  SAIBAN.
*TT  MOVE     WK-D365-DEN     TO     DEN-D99.
     MOVE     "F"             TO     WK-D365-PARA-IN1.
     MOVE     DEN-F07         TO     WK-D365-PARA-IN2.
     PERFORM  D365-DEN-SEC.
     MOVE     WK-D365-PARA-OUT1 TO   DEN-D99.
*#2020/12/21 NAV ED
*売上伝票ファイル追加
 MAIN-05.
     WRITE    DEN-REC.
     ADD      1               TO     WK-DENCNT.
*基本情報ファイル追加
 MAIN-06.
     WRITE    NFJ-REC.
     ADD      1               TO     WK-NFJCNT.
*
*伝票データ２リード
*DEN2-READ.
*    PERFORM  DEN2-RD-SEC.
*本発基本情報ＲＥＡＤ
 MAIN-99.
     PERFORM  NHJOHOL1-READ-SEC.
     IF  END-FLG1 = "END"
         MOVE    9           TO   END-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
*2020/05/25
*--------------------------------------------------------------*
*    LEVEL   1.1   採番　　　                      *
*--------------------------------------------------------------*
 SAIBAN           SECTION.
     MOVE     "SAIBAN"       TO     SEC-NAME.
*Ｄ３６５伝票編集
     ADD      1              TO     JD3-F04.
*最大値以上の場合、最小値をセット
     IF       JD3-F04   >  JD3-F06
              MOVE  JD3-F05  TO     JD3-F04
     END-IF.
*Ｄ３６５伝票番号作成
     MOVE     "F"            TO     WK-D365-TYPE.
     MOVE     DEN-F01        TO     WK-D365-TOKCD.
*    MOVE     JD3-F07        TO     WK-D365-SYSDT.
*    MOVE     WK-D365-SYSDT2 TO     WK-D365-DATE.
     MOVE     SYS-DATE       TO     WK-D365-DATE.
     MOVE     JD3-F04        TO     WK-RENBAN.
     MOVE     WK-RENBAN      TO     WK-D365-RENBAN.
*
  SAIBAN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　基本情報ファイル検索
*--------------------------------------------------------------*
 NFJOHOL1-READ-SEC    SECTION.
*
     MOVE     NHJ-F01               TO  NFJ-F01.
     MOVE     NHJ-F05               TO  NFJ-F05.
     MOVE     NHJ-F06               TO  NFJ-F06.
     MOVE     NHJ-F07               TO  NFJ-F07.
     MOVE     NHJ-F08               TO  NFJ-F08.
     MOVE     NHJ-F09               TO  NFJ-F09.
     READ     NFJOHOL1
              INVALID
                  MOVE     "INV"    TO  NFJOHOL1-INV-FLG
              NOT INVALID
                  MOVE     "   "    TO  NFJOHOL1-INV-FLG
     END-READ.
*
 NFJOHOL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　本発売上伝票ファイル検索
*--------------------------------------------------------------*
 NHTDENL1-READ-SEC    SECTION.
*           　　　　　　　　　　　　 　 取引先ＣＤ
     MOVE     NHJ-F04               TO  NHT-F01.
*           　　　　　　　　　　　　 　 自社伝票ＮＯ
     MOVE     NHJ-F07               TO  NHT-F02.
*           　　　　　　　　　　　　 　 相殺区分
     MOVE     0                     TO  NHT-F04.
*           　　　　　　　　　　　　 　 伝区ＣＤ
     MOVE     40                    TO  NHT-F051.
*           　　　　　　　　　　　　 　 店ＣＤ
     MOVE     NHJ-F06               TO  NHT-F07.
*           　　　　　　　　　　　　 　 納品日
     MOVE     NHJ-F09               TO  NHT-F112.
*           　　　　　　　　　　　　 　 行
     MOVE     NHJ-F08               TO  NHT-F03.
*
     READ     NHTDENL1
              INVALID
                  MOVE     "INV"    TO  NHTDENL1-INV-FLG
              NOT INVALID
                  MOVE     "   "    TO  NHTDENL1-INV-FLG
     END-READ.
*
 NHTDENL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*　　売上伝票ファイル検索
*--------------------------------------------------------------*
 SHTDENL1-READ-SEC    SECTION.
*           　　　　　　　　　　　　 　 取引先ＣＤ
     MOVE     NHJ-F04               TO  DEN-F01.
*           　　　　　　　　　　　　 　 自社伝票ＮＯ
     MOVE     NHJ-F07               TO  DEN-F02.
*           　　　　　　　　　　　　 　 相殺区分
     MOVE     0                     TO  DEN-F04.
*           　　　　　　　　　　　　 　 伝区ＣＤ
     MOVE     40                    TO  DEN-F051.
*           　　　　　　　　　　　　 　 店ＣＤ
     MOVE     NHJ-F06               TO  DEN-F07.
*           　　　　　　　　　　　　 　 納品日
     MOVE     NHJ-F09               TO  DEN-F112.
*           　　　　　　　　　　　　 　 行
     MOVE     NHJ-F08               TO  DEN-F03.
*
     READ     SHTDENL1
              INVALID
                  MOVE     "INV"    TO  SHTDENL1-INV-FLG
              NOT INVALID
                  MOVE     "   "    TO  SHTDENL1-INV-FLG
     END-READ.
*
 SHTDENL1-READ-EXIT.
     EXIT.
****************************************************************
*               伝票データ２ＲＥＡＤ
****************************************************************
*DEN2-RD-SEC                 SECTION.
*    MOVE     "DEN2-RD-SEC"       TO   S-NAME.
*
*    READ     SHTDENWK  AT   END
*             MOVE      9         TO   END-FLG
*    END-READ.
*
*DEN2-RD-EXIT.
*    EXIT.
****************************************************************
*               取 引 先 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
*TOK-RD-SEC                  SECTION.
*    MOVE     "TOK-RD-SEC"        TO   S-NAME.
*
*    MOVE     ZERO           TO   INV-SW
*    MOVE     WK-TOKCD       TO   TOK-F01.
*    READ     HTOKMS
*      INVALID
*             MOVE      1         TO   INV-SW
*      NOT INVALID
*             MOVE      ZERO      TO   INV-SW
*    END-READ.
*TOK-RD-EXIT.
*    EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021.
     MOVE    DEN-F1412       TO   ZAI-F022.
     MOVE    SHO-F08         TO   ZAI-F03.
     READ    ZAMZAIL1
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
      MOVE      DEN-F08       TO   ZAI-F01.
      MOVE      DEN-F1411     TO   ZAI-F021.
      MOVE      DEN-F1412     TO   ZAI-F022.
      MOVE      SHO-F08       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F27       =    ZAI-F27  +  DEN-F15.
*商品名称マスタ読込み
      PERFORM   MEIMS1-READ-SEC.
*商品名称マスタ存在チェック
      IF  MEIMS1-INV-FLG  =  SPACE
          MOVE  WK-TOKCD      TO   ZAI-F29
          MOVE  MEI-F031      TO   ZAI-F30
          MOVE  WK-DATE8      TO   ZAI-F98
          MOVE  WK-DATE8      TO   ZAI-F99
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
     COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  DEN-F15.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =    ZAI-F27  +  DEN-F15
         MOVE     WK-DATE8  TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
         MOVE     SPACE          TO   KEP-FLG
*        引当済数に数量加算
         COMPUTE  ZAI-F28   =    ZAI-F28  +  DEN-F15
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =    ZAI-F27  +  DEN-F15
         MOVE     WK-DATE8  TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 SHOTBL1-READ-SEC          SECTION.
*
     MOVE      "SHOTBL1-READ-SEC" TO    S-NAME.
*
*    MOVE      DEN-F01     TO     SHO-F01.
     MOVE      NHJ-F04     TO     SHO-F01.
*    MOVE      DEN-F25     TO     SHO-F02.
     MOVE      NHJ-F13     TO     SHO-F02.
     READ      SHOTBL1
               INVALID
               MOVE      "INV"    TO    SHOTBL1-INV-FLG
               MOVE      ZERO     TO    SHO-F09
               NOT  INVALID
               MOVE      SPACE    TO    SHOTBL1-INV-FLG
     END-READ.
*
 SHOTBL1-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
*SHTDENL1-READ-SEC          SECTION.
*
*    MOVE      "SHTDENL1-READ-SEC" TO    S-NAME.
*
*    MOVE      DEN2-F01    TO     DEN-F01.
*    MOVE      DEN2-F02    TO     DEN-F02.
*    MOVE      DEN2-F04    TO     DEN-F04.
*    MOVE      DEN2-F051   TO     DEN-F051.
*****サーバー統合時、下記２行のコメントを外すこと
*    MOVE      DEN2-F07    TO     DEN-F07.
*    MOVE      DEN2-F112   TO     DEN-F112.
*    MOVE      DEN2-F03    TO     DEN-F03.
*
*    READ      SHTDENL1
*              INVALID
*              MOVE      "INV"    TO    SHTDENL1-INV-FLG
*              NOT  INVALID
*              MOVE      SPACE    TO    SHTDENL1-INV-FLG
*    END-READ.
*
*SHTDENL1-READ-EXIT.
*    EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 MEIMS1-READ-SEC           SECTION.
*
     MOVE      "MEIMS1-READ-SEC"  TO    S-NAME.
*
     MOVE      SHO-F031    TO     MEI-F011.
     MOVE      SHO-F032    TO     MEI-F012.
     READ      MEIMS1
               INVALID
               MOVE      "INV"    TO    MEIMS1-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     DISPLAY NC"＃基本情報Ｆ作成件数　＝　" WK-NFJCNT UPON CONS.
     DISPLAY NC"＃売上伝票Ｆ作成件数　＝　" WK-DENCNT UPON CONS.
*
*    CLOSE SHTDENWK SHTDENL1 HTOKMS  SHOTBL1 MEIMS1  ZAMZAIF
*          HTANMS.
*2020/05/25
     OPEN   I-O    JYOKEN1.
     PERFORM  JYOKEN1-READ-SEC.
     IF  JYOKEN1-INV-FLG = SPACE
         MOVE  JD3-REC          TO      JYO-REC
         REWRITE JYO-REC
     ELSE
         DISPLAY NC"条件ファイルなし" "KEY1=60,KEY2=D365NONF"
                                                 UPON CONS
         MOVE  4000             TO      PROGRAM-STATUS
         MOVE    9              TO      END-FLG
     END-IF.
     CLOSE        JYOKEN1.
*
     CLOSE NHJOHOL1  NHTDENL1 NFJOHOL1 SHTDENL1 SHOTBL1 ZAMZAIL1
           MEIMS1.
*#2020/12/21 NA ST
     CLOSE  SUBTBLL1.
*#2020/12/21 NA ED
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
*TAN-READ-SEC              SECTION.
*
*    INITIALIZE                          WK-TOUSEI.
*    MOVE       SPACE          TO        TAN-REC.
*    INITIALIZE                          TAN-REC.
*    MOVE       LINK-IN-BUMON     TO        TAN-F01.
*    MOVE       LINK-IN-TANCD     TO        TAN-F02.
*    READ       HTANMS
*               INVALID
*               MOVE    "INV"  TO        TAN-INV-FLG
*               NOT INVALID
*               MOVE    SPACE  TO        TAN-INV-FLG
*    END-READ.
*
*    IF  TAN-INV-FLG   =  SPACE
*        IF   TAN-F06  = "1" OR "2" OR "3"
**************登録更新承認担当者・日付・代表権限・権限１
*             MOVE    LINK-IN-TANCD   TO    WK-DEN-F59
*             MOVE    LINK-IN-TANCD   TO    WK-DEN-F60
*             MOVE    LINK-IN-TANCD   TO    WK-DEN-F61
*             MOVE    WK-DATE8     TO    WK-DEN-F62
*             MOVE    WK-DATE8     TO    WK-DEN-F63
*             MOVE    WK-DATE8     TO    WK-DEN-F64
*             MOVE    TAN-F05      TO    WK-DEN-F65
*             MOVE    TAN-F06      TO    WK-DEN-F66
*        ELSE
**************登録更新承認担当者・日付・代表権限・権限１
*             MOVE    LINK-IN-TANCD   TO    WK-DEN-F59
*             MOVE    LINK-IN-TANCD   TO    WK-DEN-F60
*             MOVE    SPACE        TO    WK-DEN-F61
*             MOVE    WK-DATE8     TO    WK-DEN-F62
*             MOVE    WK-DATE8     TO    WK-DEN-F63
*             MOVE    ZERO         TO    WK-DEN-F64
*             MOVE    TAN-F05      TO    WK-DEN-F65
*             MOVE    TAN-F06      TO    WK-DEN-F66
*        END-IF
*    ELSE
*        DISPLAY NC"担当者Ｍ未登録" DEN2-F06 UPON CONS
*********登録更新承認担当者・日付・代表権限・権限１
*        MOVE    DEN2-F06          TO    WK-DEN-F59
*        MOVE    DEN2-F06          TO    WK-DEN-F60
*        MOVE    SPACE             TO    WK-DEN-F61
*        MOVE    WK-DATE8          TO    WK-DEN-F62
*        MOVE    WK-DATE8          TO    WK-DEN-F63
*        MOVE    ZERO              TO    WK-DEN-F64
*        MOVE    TAN-F05           TO    WK-DEN-F65
*        MOVE    TAN-F06           TO    WK-DEN-F66
*    END-IF.
*
*TAN-READ-EXIT.
*    EXIT.
*#2020/12/21 NAV ST
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 SUBTBLL1-READ-SEC          SECTION.
*
     MOVE      "SUBTBLL1-READ-SEC" TO    S-NAME.
*
     MOVE      NHJ-F04     TO     SUB-F01.
     MOVE      NHJ-F13     TO     SUB-F02.
*****DISPLAY "SUBTBL KEY = " SUB-F01 " : " SUB-F02 UPON CONS.
     READ      SUBTBLL1
               INVALID
***************DISPLAY "AAA" UPON CONS
               MOVE      "INV"    TO    SUBTBLL1-INV-FLG
               NOT  INVALID
***************DISPLAY "BBB" UPON CONS
               MOVE      SPACE    TO    SUBTBLL1-INV-FLG
     END-READ.
*
 SUBTBLL1-READ-EXIT.
     EXIT.
*#2020/12/21 NAV ED
*#2020/12/21 NAV ST
****************************************************************
*     Ｄ３６５伝票番号採番　　　　　　　                       *
****************************************************************
 D365-DEN-SEC              SECTION.
*
     CALL      "SKYD3DEN"  USING  WK-D365-PARA-IN1
                                  WK-D365-PARA-IN2
                                  WK-D365-PARA-OUT1
                                  WK-D365-PARA-OUT2.
     IF   WK-D365-PARA-OUT2  =  "1"
          DISPLAY "## " NC"異常" "D365NO" NC"採番" " ERR]] ##"
          MOVE   4000      TO      PROGRAM-STATUS
          STOP  RUN
     END-IF.
*
 D365-DEN-EXIT.
     EXIT.
*#2020/12/21 NAV ED
