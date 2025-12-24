****************************************************************
*    顧客名　　　　　　　：　_サカタのタネ殿　　　　　　　　　
*    業務名　　　　　　　：　出荷管理システム　　　　　　　　　*
*    モジュール名　　　　：　テクノス手書伝票データ作成        *
*    作成日／作成者　　　：　2011/12/2 NAV                     *
*    処理概要　　　　　　：　手書伝票基本データを順で読み、    *
*                            マスタを参照しながら、売上伝票    *
*                            ファイルを作成する。　　          *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY3739B.
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
     SELECT      SHTDENWK    ASSIGN    TO        DA-01-S-NFDENWK
                             FILE      STATUS    DEN2-ST.
*伝票データＦ
*売上伝票ファイル
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
***サーバー統合後のキーを追加　１２月開始時はコメントアウト
                                            DEN-F07   DEN-F112
*************************************************************
                                            DEN-F03
                        FILE STATUS    IS   DEN-ST.
*取引先マスタ
     SELECT      HTOKMS      ASSIGN    TO        DA-01-VI-TOKMS2
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TOK-F01
                             FILE      STATUS    TOK-ST.
*商品在庫マスタ
     SELECT     ZAMZAIF      ASSIGN    TO        DA-01-VI-ZAMZAIL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                             FILE      STATUS    ZAI-ST.
*商品コード変換テーブル
     SELECT     HSHOTBL      ASSIGN    TO        DA-01-VI-SHOTBL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       SHO-F01
                                                 SHO-F02
                             FILE      STATUS    SHO-ST.
*商品名称マスタ
     SELECT     HMEIMS       ASSIGN    TO        DA-01-VI-MEIMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       MEI-F01
                             FILE      STATUS    MEI-ST.
*担当者マスタ
     SELECT     HTANMS       ASSIGN    TO        DA-01-VI-TANMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TAN-F01  TAN-F02
                             FILE      STATUS    TAN-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*伝票データＦ
 FD  SHTDENWK
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        NFDENWK     OF        XFDLIB
     JOINING     DEN2        AS        PREFIX.
*伝票データＦ
 FD  SHTDENF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
     COPY        SHTDENF     OF        XFDLIB
     JOINING     DEN         AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS     8        RECORDS.
     COPY        HTOKMS      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
*商品在庫マスタ
 FD  ZAMZAIF.
     COPY        ZAMZAIF     OF        XFDLIB
                 JOINING     ZAI       PREFIX.
*商品変換テーブル
 FD  HSHOTBL.
     COPY        HSHOTBL     OF        XFDLIB
                 JOINING     SHO       PREFIX.
*商品名称マスタ
 FD  HMEIMS.
     COPY        HMEIMS      OF        XFDLIB
                 JOINING     MEI       PREFIX.
*担当者マスタ
 FD  HTANMS.
     COPY        HTANMS      OF        XFDLIB
                 JOINING     TAN       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  DEN2-ST             PIC  X(02)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  INV-SW              PIC  9(01)  VALUE  ZERO.
     03  WK-DENCNT           PIC  9(09)  VALUE  ZERO.
     03  IX                  PIC  9(01)  VALUE  ZERO.
     03  WK-TOKCD            PIC  9(08)  VALUE  ZERO.
     03  WK-TENCD            PIC  9(05)  VALUE  ZERO.
     03  WK-DENNO            PIC  9(09)  VALUE  ZERO.
*\\\ 93.06.04 START \\\
     03  WK-SYUKA            PIC  9(08)  VALUE  ZERO.
     03  WK-BASYO            PIC  X(02)  VALUE  SPACE.
*\\\ 93.06.04 END   \\\
     03  WK-TOKNM            PIC  N(10).
     03  L-CNT               PIC  99     VALUE  ZERO.
     03  P-CNT               PIC  99     VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  BIKO-INV-FLG        PIC  X(03)  VALUE  SPACE.
     03  WK-DENBIKO          PIC  9(09)  VALUE  ZERO.
     03  WK-DENPYO           PIC  9(09)  VALUE  ZERO.
     03  WK-TORBIKO          PIC  9(08)  VALUE  ZERO.
     03  TAN-INV-FLG         PIC  X(03)  VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)  VALUE  SPACE.
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
     03  DEN2-ERR            PIC  N(10)  VALUE
                   NC"伝票データＦ２異常".
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"伝票データＦ異常".
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
 01  LINK-WKSTNNM                PIC  X(08).
 01  LINK-BUMON                  PIC  X(04).
 01  LINK-TANCD                  PIC  X(02).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-WKSTNNM
                                             LINK-BUMON
                                             LINK-TANCD.
****************************************************************
 DECLARATIVES.
 DEN2-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENWK.
     DISPLAY     DEN2-ERR    UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN2-ST     UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     DEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TAN-ST      UPON      CONS.
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
     OPEN        I-O         SHTDENWK  HTOKMS  ZAMZAIF
                 INPUT       HSHOTBL   HMEIMS  HTANMS
                 I-O         SHTDENF.
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
     PERFORM   TAN-READ-SEC.
*量販ＲＥＣ初期化
     MOVE        SPACE       TO   DEN-REC.
     INITIALIZE  DEN-REC.
 INIT-010.
*伝票データ２初期読み
     PERFORM  DEN2-RD-SEC.
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*伝票データ作成
*****売上伝票ファイルを検索する
     PERFORM  SHTDENF-READ-SEC.
*既に登録済の場合は更新しない。
     IF  SHTDENF-INV-FLG = SPACE
         GO                  TO        DEN2-READ
     END-IF.
*
     MOVE     DEN2-REC       TO        DEN-REC.
     MOVE     DEN2-F02       TO        DEN-F23.
     MOVE     ZERO           TO        DEN-F113.
 MAIN061.
*****内部統制対応
     MOVE     WK-DEN-F59     TO        DEN-F59.
     MOVE     WK-DEN-F60     TO        DEN-F60.
     MOVE     WK-DEN-F61     TO        DEN-F61.
     MOVE     WK-DEN-F62     TO        DEN-F62.
     MOVE     WK-DEN-F63     TO        DEN-F63.
     MOVE     WK-DEN-F64     TO        DEN-F64.
     MOVE     WK-DEN-F65     TO        DEN-F65.
     MOVE     WK-DEN-F66     TO        DEN-F66.
     MOVE     ZERO           TO        DEN-F67.
     MOVE     LINK-TANCD     TO        DEN-F06.
*
 MAIN062.
*商品変換ＴＢＬチェック*
     PERFORM     HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG  =  SPACE
*        *商品名称マスタチェック*
         MOVE    SHO-F031  TO        MEI-F011
         MOVE    SHO-F032  TO        MEI-F012
         PERFORM  HMEIMS-READ-SEC
     END-IF.
*仕入単価取得
     IF     SHO-F09   =  ZERO
         MOVE    MEI-F041  TO        DEN-F171
     ELSE
         MOVE    SHO-F09   TO        DEN-F171
     END-IF.
*在庫引当
     IF  HSHOTBL-INV-FLG  =  SPACE
         PERFORM  ZAIKO-SEC
         IF  KEP-FLG  =  SPACE
             MOVE     1       TO     DEN-F27D
         END-IF
         MOVE         1       TO     DEN-F262
     END-IF.
*伝票データ追加
 MAIN-010.
     WRITE    DEN-REC.
     ADD      1               TO     WK-DENCNT.
*伝票データ２リード
 DEN2-READ.
     PERFORM  DEN2-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*               伝票データ２ＲＥＡＤ
****************************************************************
 DEN2-RD-SEC                 SECTION.
     MOVE     "DEN2-RD-SEC"       TO   S-NAME.
*
     READ     SHTDENWK  AT   END
              MOVE      9         TO   END-FLG
     END-READ.
*
 DEN2-RD-EXIT.
     EXIT.
****************************************************************
*               取 引 先 マ ス タ Ｒ Ｅ Ａ Ｄ
****************************************************************
 TOK-RD-SEC                  SECTION.
     MOVE     "TOK-RD-SEC"        TO   S-NAME.
*
     MOVE     ZERO           TO   INV-SW
     MOVE     WK-TOKCD       TO   TOK-F01.
     READ     HTOKMS
       INVALID
              MOVE      1         TO   INV-SW
       NOT INVALID
              MOVE      ZERO      TO   INV-SW
     END-READ.
 TOK-RD-EXIT.
     EXIT.
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
     READ    ZAMZAIF
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
      PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
      IF  HMEIMS-INV-FLG  =  SPACE
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
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      DEN-F01     TO     SHO-F01.
     MOVE      DEN-F25     TO     SHO-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
*2009.09.14
               MOVE      ZERO     TO    SHO-F09
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 SHTDENF-READ-SEC          SECTION.
*
     MOVE      "SHTDENF-READ-SEC" TO    S-NAME.
*
     MOVE      DEN2-F01    TO     DEN-F01.
     MOVE      DEN2-F02    TO     DEN-F02.
     MOVE      DEN2-F04    TO     DEN-F04.
     MOVE      DEN2-F051   TO     DEN-F051.
*****サーバー統合時、下記２行のコメントを外すこと
     MOVE      DEN2-F07    TO     DEN-F07.
     MOVE      DEN2-F112   TO     DEN-F112.
     MOVE      DEN2-F03    TO     DEN-F03.
*
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
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     DISPLAY NC"＃売上伝票Ｆ作成件数　＝　" WK-DENCNT UPON CONS.
*
     CLOSE SHTDENWK SHTDENF HTOKMS HSHOTBL HMEIMS ZAMZAIF
           HTANMS.
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TAN-READ-SEC              SECTION.
*
     INITIALIZE                          WK-TOUSEI.
     MOVE       SPACE          TO        TAN-REC.
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
              MOVE    LINK-TANCD   TO    WK-DEN-F59
              MOVE    LINK-TANCD   TO    WK-DEN-F60
              MOVE    LINK-TANCD   TO    WK-DEN-F61
              MOVE    WK-DATE8     TO    WK-DEN-F62
              MOVE    WK-DATE8     TO    WK-DEN-F63
              MOVE    WK-DATE8     TO    WK-DEN-F64
              MOVE    TAN-F05      TO    WK-DEN-F65
              MOVE    TAN-F06      TO    WK-DEN-F66
         ELSE
**************登録更新承認担当者・日付・代表権限・権限１
              MOVE    LINK-TANCD   TO    WK-DEN-F59
              MOVE    LINK-TANCD   TO    WK-DEN-F60
              MOVE    SPACE        TO    WK-DEN-F61
              MOVE    WK-DATE8     TO    WK-DEN-F62
              MOVE    WK-DATE8     TO    WK-DEN-F63
              MOVE    ZERO         TO    WK-DEN-F64
              MOVE    TAN-F05      TO    WK-DEN-F65
              MOVE    TAN-F06      TO    WK-DEN-F66
         END-IF
     ELSE
         DISPLAY NC"担当者Ｍ未登録" DEN2-F06 UPON CONS
*********登録更新承認担当者・日付・代表権限・権限１
         MOVE    DEN2-F06          TO    WK-DEN-F59
         MOVE    DEN2-F06          TO    WK-DEN-F60
         MOVE    SPACE             TO    WK-DEN-F61
         MOVE    WK-DATE8          TO    WK-DEN-F62
         MOVE    WK-DATE8          TO    WK-DEN-F63
         MOVE    ZERO              TO    WK-DEN-F64
         MOVE    TAN-F05           TO    WK-DEN-F65
         MOVE    TAN-F06           TO    WK-DEN-F66
     END-IF.
*
 TAN-READ-EXIT.
     EXIT.
