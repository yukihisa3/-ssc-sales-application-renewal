****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　D365連携データ作成(移動依頼)
*    作成日／作成者　　　：　2020/02/24   ASS.II               *
*    処理内容　　　　　　：　新入出ﾌｧｲﾙよりD365連携データ作
*    作成日／作成者　　　：　2020/05/29   NAV TAKAHASHI        *
*    処理内容　　　　　　：　新移動依頼に変更
*    作成日／作成者　　　：　2021/11/04   NAV TAKAHASHI        *
*    処理内容　　　　　　：　出力Ｆにキー情報付加
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0450B.
 AUTHOR.                ASS.II.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<  新入出庫ファイル  >>******************************
     SELECT   DNSFILF   ASSIGN    TO      DA-01-VI-DNSFILL3
                        FILE STATUS          IS  DNS-STATUS
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  DNS-F22
                                                 DNS-F12
                                                 DNS-F01
                                                 DNS-F02.
****<< サブ商品名称マスタ >>********************************
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F011
                                                 SUB-F0121
                                                 SUB-F0122
                                                 SUB-F0123
                        FILE      STATUS    IS   SUB-STATUS.
****<< 倉庫マスタ　　　　 >>********************************
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
****<< 条件ファイル　　　 >>********************************
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
****<< 移動実績データ >>************************************
     SELECT   JISIDOF   ASSIGN    TO        DA-01-S-JISIDOF
                        FILE      STATUS    IS   IDO-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 新入出庫ファイル >>*********************************
 FD  DNSFILF.
     COPY     DNSFILF   OF        XFDLIB
              JOINING   DNS       PREFIX.
****<< サブ商品名称マスタ >>*******************************
 FD  SUBMEIF.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SUB       PREFIX.
****<< 倉庫マスタ　　　　 >>*******************************
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
****<< 条件ファイル　　　 >>*******************************
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****<< 移動実績データ >>***********************************
 FD  JISIDOF.
     COPY     JISIDOF   OF        XFDLIB
              JOINING   IDO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報
 01  STATUS-AREA.
     02 DNS-STATUS           PIC  X(02).
     02 SUB-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
     02 IDO-STATUS           PIC  X(02).
****  システム日付・時刻
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC  9(06).
     03  WK-SYSDATE          PIC  9(08).
     03  SYSHMS.
         05  WK-SYSTIME      PIC  9(06).
****  フラグ
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  SUBMEIF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  ZSOKMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)  VALUE  SPACE.
****  カウント
 01  CNT-AREA.
     03  IN-CNT              PIC  9(10)  VALUE  ZERO.
     03  IN-CNT-R            REDEFINES  IN-CNT.
         05  FILLER          PIC  X(07).
         05  IN-CNT-X        PIC  X(03).
     03  SEL-CNT             PIC  9(10)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(10)  VALUE  ZERO.
****  退避エラア
*****COPY   DNSFILF
************JOINING  SAV  AS PREFIX.
     COPY   DNSFILF  OF XFDLIB  JOINING   SAV  AS   PREFIX.
*    COPY     DNSFILF   OF        XFDLIB
*             JOINING   SAV       PREFIX.
****  処理件数表示
 01  DSP-CNT-AREA.
     03  DSP-READ-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** DNSFILL3   (READ) =".
         05  DSP-READ        PIC  Z,ZZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(15) VALUE
         " * 処理中]]".
     03  DSP-IN-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** DNSFILL3  (INPUT) =".
         05  DSP-IN          PIC  Z,ZZZ,ZZZ,ZZ9.
     03  DSP-OUT-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** JISIDOF  (OUTPUT) =".
         05  DSP-OUT         PIC  Z,ZZZ,ZZZ,ZZ9.

**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0450B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  PARA-CNT                PIC  9(10).
****************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-CNT.
****************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DNSFILF.
     MOVE   "DNSFILF"         TO    ERR-FL-ID.
     MOVE    DNS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIF.
     MOVE   "SUBMEIL1"        TO    ERR-FL-ID.
     MOVE    SUB-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSOKMS.
     MOVE   "ZSOKMS11"        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "JYOKEN1 "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JISIDOF.
     MOVE   "JISIDOF"         TO    ERR-FL-ID.
     MOVE    IDO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
                            UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理
************************************************************
 100-INIT-SEC           SECTION.

*システム日付・時刻の取得
     ACCEPT   SYSYMD     FROM     DATE.
     ACCEPT   SYSHMS     FROM     TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-SYSDATE.

     OPEN     I-O       DNSFILF.
     OPEN     INPUT     SUBMEIF  ZSOKMS  HJYOKEN.
     OPEN     OUTPUT    JISIDOF.

*入出庫ファイルの読込
     PERFORM  DNSFILF-READ-SEC.

 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理
************************************************************
 200-MAIN-SEC           SECTION.

*サブ商品名称マスタ／検索
     MOVE     DNS-F03         TO   SUB-F011.    *> 商品ＣＤ
     MOVE     DNS-F04         TO   SUB-F012.    *> 品単ＣＤ
     PERFORM  SUBMEIF-READ-SEC.

*移動実績データ／編集
     MOVE     SPACE               TO   IDO-REC.
     INITIALIZE                        IDO-REC.
*  作業実績日
*#2021/08/31 NAV ST
*****MOVE     DNS-F14(1:4)        TO   IDO-F01(1:4).
*    MOVE     "/"                 TO   IDO-F01(5:1).
*    MOVE     DNS-F14(5:2)        TO   IDO-F01(6:2).
*    MOVE     "/"                 TO   IDO-F01(8:1).
*****MOVE     DNS-F14(7:2)        TO   IDO-F01(9:2).
****
     MOVE     DNS-F18(1:4)        TO   IDO-F01(1:4).
     MOVE     "/"                 TO   IDO-F01(5:1).
     MOVE     DNS-F18(5:2)        TO   IDO-F01(6:2).
     MOVE     "/"                 TO   IDO-F01(8:1).
     MOVE     DNS-F18(7:2)        TO   IDO-F01(9:2).
*#2021/08/31 NAV ED
     IF  SUBMEIF-INV-FLG  =  SPACE
*  新商品コード
*  旧商品コード
*  ＪＡＮＣＤ
         MOVE   SUB-D01           TO   IDO-F04
     ELSE
         DISPLAY NC"＃ＪＡＮＣＤ取得エラー＃＃" UPON CONS
         DISPLAY NC"＃サカタ商品" " = " DNS-F03 "-" DNS-F04
         UPON CONS
         MOVE   4000              TO   PROGRAM-STATUS
         STOP  RUN
     END-IF.
*  ストックＮｏ
     MOVE     SPACE               TO   IDO-F05.
*  移動元倉庫ＣＤ
*    倉庫　　　　　　　
     MOVE      DNS-F05       TO   SOK-F01.
     PERFORM   ZSOKMS-READ-SEC.
     IF   ZSOKMS-INV-FLG  =  "INV"
          MOVE "0"           TO   IDO-F05(1:1)
          MOVE  DNS-F05      TO   IDO-F05(2:2)
          MOVE  "00"         TO   IDO-F05
     ELSE
**********Ｄ３６５倉庫ＣＤ
          MOVE  SOK-F16      TO   IDO-F05
**********保管場所取得
          MOVE  42           TO   JYO-F01
          MOVE  SOK-F17      TO   JYO-F02
          PERFORM  HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = "INV"
              MOVE  "00"     TO   IDO-F06
          ELSE
              MOVE   JYO-F14 TO   IDO-F06
          END-IF
     END-IF.
*  移動先倉庫ＣＤ
*    倉庫　　　　　　　
     MOVE      DNS-F08       TO   SOK-F01.
     PERFORM   ZSOKMS-READ-SEC.
     IF   ZSOKMS-INV-FLG  =  "INV"
          MOVE "0"           TO   IDO-F07(1:1)
          MOVE  DNS-F05      TO   IDO-F07(2:2)
          MOVE  "00"         TO   IDO-F07
     ELSE
**********Ｄ３６５倉庫ＣＤ
          MOVE  SOK-F16      TO   IDO-F07
**********保管場所取得
          MOVE  42           TO   JYO-F01
          MOVE  SOK-F17      TO   JYO-F02
          PERFORM  HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = "INV"
              MOVE  "00"     TO   IDO-F08
          ELSE
              MOVE   JYO-F14 TO   IDO-F08
          END-IF
     END-IF.
*  数量
     COMPUTE  IDO-F09  =  DNS-F15  *  100.
*#2021/11/04 NAV ST
*  伝票番号
     MOVE     DNS-F01             TO  IDO-F10.
*  行番号
     MOVE     DNS-F02             TO  IDO-F11.
*#2021/11/04 NAV ED

*移動実績データ／出力
     WRITE    IDO-REC.
     ADD      1                   TO   OUT-CNT.


*入出庫ファイル／更新
     MOVE     "1"                 TO   DNS-F22.
     MOVE     WK-SYSDATE          TO   DNS-F23.
     MOVE     WK-SYSTIME          TO   DNS-F24.
     REWRITE  DNS-REC.


*次の入出庫ファイルの読込
     PERFORM  DNSFILF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    入出庫ファイルの読込処理
************************************************************
 DNSFILF-READ-SEC       SECTION.

 DNSFILF-READ-100.
     READ    DNSFILF   NEXT
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        DNSFILF-READ-EXT
     END-READ.

     ADD     1                   TO   IN-CNT.
     IF     IN-CNT-X  =  "500"  OR  "000"
            MOVE   IN-CNT             TO   DSP-READ
            DISPLAY  DSP-READ-AREA  UPON   CONS
     END-IF.

*Ｄ３６５計上区分＝空白が対象
     IF     DNS-F22  =  SPACE
            CONTINUE
     ELSE
            GO          TO       DNSFILF-READ-100
     END-IF.

*出庫完了区分入庫完了区分チェック
     IF     DNS-F17  =  "1"
     AND    DNS-F21  =  "1"
            CONTINUE
     ELSE
            GO          TO       DNSFILF-READ-100
     END-IF.

     ADD     1                   TO   SEL-CNT.

 DNSFILF-READ-EXT.
     EXIT.
************************************************************
*      サブ商品名称マスタの読込
************************************************************
 SUBMEIF-READ-SEC       SECTION.
     READ    SUBMEIF
       INVALID      KEY
          MOVE      "INV"        TO   SUBMEIF-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   SUBMEIF-INV-FLG
     END-READ.
 SUBMEIF-READ-EXT.
     EXIT.
************************************************************
*      倉庫マスタの読込
************************************************************
 ZSOKMS-READ-SEC        SECTION.
     READ    ZSOKMS
       INVALID      KEY
          MOVE      "INV"        TO   ZSOKMS-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   ZSOKMS-INV-FLG
     END-READ.
 ZSOKMS-READ-EXT.
     EXIT.
************************************************************
*      条件ファイルの読込
************************************************************
 HJYOKEN-READ-SEC       SECTION.
     READ    HJYOKEN
       INVALID      KEY
          MOVE      "INV"        TO   HJYOKEN-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   HJYOKEN-INV-FLG
     END-READ.
 HJYOKEN-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理
************************************************************
 300-END-SEC            SECTION.

     MOVE   IN-CNT               TO   DSP-IN.
     MOVE   OUT-CNT              TO   DSP-OUT.
     DISPLAY  DSP-IN-AREA      UPON   CONS.
     DISPLAY  DSP-OUT-AREA     UPON   CONS.

     MOVE   OUT-CNT              TO   PARA-CNT.

*#### TEST <<<<<<
     DISPLAY " 移動実績作成件数= "  OUT-CNT   UPON CONS.
*#### TEST >>>>>>

     CLOSE  DNSFILF
            SUBMEIF
            JISIDOF.
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
