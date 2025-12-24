****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　入出庫機能　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　入出庫機能　　　　　*
*    モジュール名　　　　：　ＮＡＶＳ間移動先発注ＤＴ作成　　　*
*    作成日／作成者　　　：　2020/05/08 TAKAHASHI              *
*    処理概要　　　　　　：　新入出庫Ｆを読み、対象条件の発注  *
*                            ＨＥＤ、ＭＥＩを作成する。　　　　*
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVD0550B.
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
*新入出庫ファイル　
     SELECT   DNSFILF   ASSIGN    TO        DA-01-VI-DNSFILL8
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DNS-F09   DNS-F29
                                            DNS-F01   DNS-F02
                        FILE  STATUS   IS   DNS-ST.
*ＳＵＢ商品名称マスタ
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SBM-F011  SBM-F0121
                                            SBM-F0122 SBM-F0123
                        FILE STATUS    IS   MEI-ST.
*発注ヘッダファイル
     SELECT   HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HED-F02
                        FILE STATUS    IS   HED-ST.
*発注明細ファイル
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F02  MEI-F03
                        FILE STATUS    IS   MEI-ST.
*商品在庫マスタ
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01
                                            ZAI-F02
                                            ZAI-F03
                        FILE STATUS    IS   ZAI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*新入出庫ファイルタ
 FD  DNSFILF
                        LABEL RECORD   IS   STANDARD.
     COPY     DNSFILF   OF        XFDLIB
              JOINING   DNS  AS   PREFIX.
*ＳＵＢ商品名称マスタ
 FD  SUBMEIF
                        LABEL RECORD   IS   STANDARD.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SBM  AS   PREFIX.
*発注ヘッダファイル
 FD  HACHEDF
                        LABEL RECORD   IS   STANDARD.
     COPY     HACHEDF   OF        XFDLIB
              JOINING   HED  AS   PREFIX.
*発注明細ファイル
 FD  HACMEIF
                        LABEL RECORD   IS   STANDARD.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   MEI  AS   PREFIX.
*商品在庫マスタ
 FD  ZAMZAIF
                        LABEL RECORD   IS   STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI  AS   PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  DNS-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  SBM-ST              PIC  X(02)  VALUE  SPACE.
     03  HED-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  INV-SW              PIC  9(01)  VALUE  ZERO.
     03  WK-RD-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-HD-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-ME-CNT           PIC  9(09)  VALUE  ZERO.
     03  SUBMEIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  ZAMZAIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HACHEDF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  HACMEIF-INV-FLG     PIC  X(03)  VALUE  SPACE.
*ブレイクキー
 01  WRK-KEY.
     03  WRK-F01             PIC  9(07)  VALUE  ZERO.
     03  WRK-F02             PIC  9(02)  VALUE  ZERO.
*
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
*
 01  FILE-ERR.
     03  DNS-ERR             PIC  N(10)  VALUE
                   NC"新入出庫Ｆ　　　異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件Ｆ　　　　　異常".
     03  SBM-ERR             PIC  N(10)  VALUE
                   NC"ＳＵＢ商品名称Ｍ異常".
     03  HED-ERR             PIC  N(10)  VALUE
                   NC"発注ヘッダＦ　　異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"発注明細Ｆ　　　異常".
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ　　　異常".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 DNS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DNSFILF.
     DISPLAY     DNS-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DNS-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SBM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBMEIF.
     DISPLAY     SBM-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SBM-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 HED-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HACHEDF.
     DISPLAY     HED-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HED-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HACMEIF.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        I-O         HACHEDF  HACMEIF  ZAMZAIF  DNSFILF
                 INPUT       SUBMEIF.
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
*新入出庫ファイルスタート
     MOVE        SPACE       TO   DNS-REC.
     INITIALIZE  DNS-REC.
     MOVE        SPACE       TO   DNS-F09.
     MOVE        SPACE       TO   DNS-F29.
     START DNSFILF  KEY  IS  >=  DNS-F09  DNS-F29  DNS-F01
                                 DNS-F02
             INVALID
             MOVE  "END"           TO   END-FLG
             DISPLAY NC"＃対象ＤＴ無し（ＳＴ）＃"  UPON  CONS
             GO                    TO   INIT-EXIT
     END-START.
 INIT-010.
*基本売上伝票データ
     PERFORM  DNS-RD-SEC.
*
     IF  END-FLG  =  "END"
         DISPLAY NC"＃対象ＤＴ無し（初回ＲＤ）＃"  UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
     IF   DNS-F01  =  WRK-F01
*****AND  DNS-F02  =  WRK-F02
          CONTINUE
     ELSE
**********発注ヘッダファイル作成
          PERFORM  HACHEDF-WT-SEC
          MOVE   DNS-F01          TO   WRK-F01
          MOVE   DNS-F02          TO   WRK-F02
     END-IF.
*発注明細データ作成
     PERFORM  HACMEIF-WT-SEC.
*
 MEI010.
     PERFORM  DNS-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    新入出庫ファイル読込　　　　　　　　
****************************************************************
 DNS-RD-SEC                 SECTION.
     MOVE     "DNS-RD-SEC"        TO   S-NAME.
*
     READ     DNSFILF   AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNS-RD-EXIT
              NOT  AT  END
              ADD       1         TO   WK-RD-CNT
     END-READ.
*
     IF  WK-RD-CNT(7:3)  =  "000"  OR  "500"
         DISPLAY "## READ-CNT = " WK-RD-CNT UPON CONS
     END-IF.
*対象判定（入庫倉庫区分、入庫完了区分が空白の時）
     IF  DNS-F09  =  SPACE
         CONTINUE
     ELSE
         MOVE  "END"              TO   END-FLG
         GO                       TO   DNS-RD-EXIT
     END-IF.
*対象判定（入庫倉庫区分、入庫完了区分が空白の時）
     IF  DNS-F29  =  SPACE
         CONTINUE
     ELSE
         MOVE  "END"              TO   END-FLG
         GO                       TO   DNS-RD-EXIT
     END-IF.
*出庫完了区分＝１のデータが対象
     IF  DNS-F17  =  "1"
         CONTINUE
     ELSE
         GO                       TO   DNS-RD-SEC
     END-IF.
*伝票番号が7000000以上が対象
     IF  DNS-F01  >= 7000000
         CONTINUE
     ELSE
         GO                       TO   DNS-RD-SEC
     END-IF.
*
 DNS-RD-EXIT.
     EXIT.
****************************************************************
*               発注ヘッダファイル作成
****************************************************************
 HACHEDF-WT-SEC              SECTION.
     MOVE     "HACHEDF-WT-SEC"    TO   S-NAME.
*
     MOVE     DNS-F01        TO   HED-F02.
     READ  HACHEDF
           INVALID     MOVE  "INV"     TO   HACHEDF-INV-FLG
           NOT INVALID MOVE  SPACE     TO   HACHEDF-INV-FLG
     END-READ.
*
     IF    HACHEDF-INV-FLG  =  SPACE
           DISPLAY NC"＃同一キーが存在します！＃" UPON CONS
           DISPLAY NC"＃伝票番号" " = " DNS-F01   UPON CONS
           GO                TO   HACHEDF-WT-EXIT
     END-IF.
*
     MOVE     SPACE          TO   HED-REC.
     INITIALIZE                   HED-REC.
*伝票区分
     MOVE     50             TO   HED-F01.
*伝票番号
     MOVE     DNS-F01        TO   HED-F02.
*担当者
     MOVE     DNS-F11        TO   HED-F05.
*仕入先
     MOVE     "NAVSNYUK"     TO   HED-F06.
*発注日
     MOVE     DNS-F12        TO   HED-F10.
*納入予定日
     MOVE     DNS-F12        TO   HED-F11.
*倉庫ＣＤ
     MOVE     DNS-F08        TO   HED-F17.
*税区分
     MOVE     0              TO   HED-F13.
*発注区分／発行区分
     MOVE     "1"            TO   HED-F25  HED-F97.
*データ種別
     MOVE     43             TO   HED-F311.
*Ｄ３６５伝票番号
     MOVE     DNS-F01        TO   HED-F313.
*登録日
     MOVE     WK-DATE8       TO   HED-F98.
*更新日
     MOVE     WK-DATE8       TO   HED-F99.
*ヘッダ作成
     WRITE  HED-REC.
     ADD      1              TO   WK-HD-CNT.
*
 HACHEDF-WT-EXIT.
     EXIT.
****************************************************************
*               発注明細ファイル作成
****************************************************************
 HACMEIF-WT-SEC              SECTION.
     MOVE     "HACMEIF-WT-SEC"    TO   S-NAME.
*
     MOVE     DNS-F01        TO   MEI-F02.
     MOVE     DNS-F02        TO   MEI-F03.
     READ  HACMEIF
           INVALID     MOVE  "INV"     TO   HACMEIF-INV-FLG
           NOT INVALID MOVE  SPACE     TO   HACMEIF-INV-FLG
     END-READ.
*
     IF    HACMEIF-INV-FLG  =  SPACE
           DISPLAY NC"＃同一キーが存在します！＃" UPON CONS
           DISPLAY NC"＃伝票番号" " = " DNS-F01   UPON CONS
           DISPLAY NC"＃行番号　" " = " DNS-F02   UPON CONS
           GO                TO   HACMEIF-WT-EXIT
     END-IF.
*
     MOVE     SPACE          TO   MEI-REC.
     INITIALIZE                   MEI-REC.
*伝票区分
     MOVE     50             TO   MEI-F01.
*伝票番号
     MOVE     DNS-F01        TO   MEI-F02.
*行番号
     MOVE     DNS-F02        TO   MEI-F03.
*商品ＣＤ
     MOVE     DNS-F03        TO   MEI-F06.
*品単ＣＤ　
     MOVE     DNS-F04        TO   MEI-F07.
*_番
     MOVE     DNS-F10        TO   MEI-F08.
*発注数
     MOVE     DNS-F13        TO   MEI-F09.
*単価区分
     MOVE     1              TO   MEI-F11.
*納入日　
     MOVE     DNS-F12        TO   MEI-F17.
*発注日
     MOVE     DNS-F12        TO   MEI-F19.
*データ種別
     MOVE     43             TO   MEI-F22.
*Ｄ３６５伝票番号
     MOVE     DNS-F01        TO   MEI-F21.
*登録日
     MOVE     WK-DATE8       TO   MEI-F98.
*更新日
     MOVE     WK-DATE8       TO   MEI-F99.
*在庫更新
     PERFORM  ZAIKO-SEC.
*明細作成
     WRITE  MEI-REC.
     ADD      1              TO   WK-ME-CNT.
*
     MOVE     1              TO   DNS-F29.
     REWRITE  DNS-REC.
*
 HACMEIF-WT-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    DNS-F08         TO   ZAI-F01.
     MOVE    DNS-F03         TO   ZAI-F021.
     MOVE    DNS-F04         TO   ZAI-F022.
     MOVE    DNS-F10         TO   ZAI-F03.
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
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      DNS-F08       TO   ZAI-F01.
      MOVE      DNS-F03       TO   ZAI-F021.
      MOVE      DNS-F04       TO   ZAI-F022.
      MOVE      DNS-F10       TO   ZAI-F03.
*未入庫数加算
      COMPUTE   ZAI-F26       =    ZAI-F26  +  DNS-F13.
*商品名称マスタ読込み
      PERFORM   SUBMEIF-READ-SEC.
*T
*     DISPLAY "SUBMEIF-INV-FLG=" SUBMEIF-INV-FLG UPON CONS.
*T
*商品名称マスタ存在チェック
      IF  SUBMEIF-INV-FLG  =  SPACE
          MOVE  SBM-F031      TO   ZAI-F30
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
*    未入庫数加算
     COMPUTE   ZAI-F26       =    ZAI-F26  +  DNS-F13.
     MOVE     WK-DATE8  TO   ZAI-F99.
*    商品在庫マスタ更新
     REWRITE  ZAI-REC.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*              ＳＵＢ商品名称マスタ読込                        *
****************************************************************
 SUBMEIF-READ-SEC          SECTION.
*
     MOVE      "SUBMEIF-READ-SEC" TO    S-NAME.
*
     MOVE      DNS-F03      TO    SBM-F011.
     MOVE      DNS-F04(1:5) TO    SBM-F0121.
     MOVE      DNS-F04(6:2) TO    SBM-F0122.
     MOVE      DNS-F04(8:1) TO    SBM-F0123.
     READ      SUBMEIF
               INVALID
               MOVE      "INV"    TO    SUBMEIF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SUBMEIF-INV-FLG
     END-READ.
*
 SUBMEIF-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     DISPLAY "READ-CNT  = "  WK-RD-CNT  UPON CONS.
     DISPLAY "HED-CNT   = "  WK-HD-CNT  UPON CONS.
     DISPLAY "MEI-CNT   = "  WK-ME-CNT  UPON CONS.
*
     CLOSE       HACHEDF  HACMEIF  ZAMZAIF
                 SUBMEIF  DNSFILF.
 END-EXIT.
     EXIT.
