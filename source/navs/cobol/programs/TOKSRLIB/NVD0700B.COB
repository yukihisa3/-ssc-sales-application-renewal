****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　Ｄ３６５連携データ作成（廃棄実績）*
*    処理概要　　　　　　：　入出庫ファイルより廃棄実績データを*
*    　　　　　　　　　　　　作成する。　　　　　　　　　　　　*
*    作成日／作成者　　　：　2019/12/25   NAV TAKAHASHI        *
*    更新日／更新者　　　：　YYYY/MM/DD                        *
*    　　変更概要　　　　：                                    *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0700B.
 AUTHOR.                NAV.
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
****<< 入出庫ファイル >>*********************************
     SELECT   NYSFILF   ASSIGN    TO        DA-01-VI-NYSFILL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   NYS-F02
                                                 NYS-F03
                        FILE      STATUS    IS   NYS-STATUS.
****<< サブ商品名称マスタ >>********************************
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F011
                                                 SUB-F0121
                                                 SUB-F0122
                                                 SUB-F0123
                        FILE      STATUS    IS   SUB-STATUS.
*倉庫マスタ
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE  STATUS   IS   SOK-STATUS.
*
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE  STATUS   IS   JYO-STATUS.
*
****<< 廃棄実績データ >>************************************
     SELECT   SNDHIKF   ASSIGN    TO        DA-01-S-SNDHIKF
                        FILE      STATUS    IS   SND-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 入出庫ファイル >>*********************************
 FD  NYSFILF.
     COPY     NYSFILF   OF        XFDLIB
              JOINING   NYS       PREFIX.
****<< サブ商品名称マスタ >>*******************************
 FD  SUBMEIF.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SUB       PREFIX.
*
******************************************************************
*    倉庫マスタ
******************************************************************
 FD  ZSOKMS             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****<< 廃棄実績データ >>***********************************
 FD  SNDHIKF.
     COPY     SNDNJYTF  OF        XFDLIB
              JOINING   SND       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報
 01  STATUS-AREA.
     02 NYS-STATUS           PIC  X(02).
     02 SUB-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
     02 SND-STATUS           PIC  X(02).
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
 01  WK-HIK-KOKYAKU          PIC  X(15)  VALUE  SPACE.
 01  WK-HIK-NOUNYU           PIC  X(15)  VALUE  SPACE.
****  カウント
 01  CNT-AREA.
     03  IN-CNT              PIC  9(10)  VALUE  ZERO.
     03  IN-CNT-R            REDEFINES  IN-CNT.
         05  FILLER          PIC  X(07).
         05  IN-CNT-X        PIC  X(03).
     03  SEL-CNT             PIC  9(10)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(10)  VALUE  ZERO.
***** 店舗CD変換
 01  WK-TENPO-CD             PIC   X(06).
 01  WK-TENPO-CD-R           REDEFINES  WK-TENPO-CD.
     03  WK-TENPO-HEN        PIC   9(06).
****  処理件数表示
 01  DSP-CNT-AREA.
     03  DSP-READ-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** NYSFILL1   (READ) =".
         05  DSP-READ        PIC  Z,ZZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(15) VALUE
         " * 処理中]]".
     03  DSP-IN-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** NYSFILL1  (INPUT) =".
         05  DSP-IN          PIC  Z,ZZZ,ZZZ,ZZ9.
     03  DSP-OUT-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** SNDHIKF  (OUTPUT) =".
         05  DSP-OUT         PIC  Z,ZZZ,ZZZ,ZZ9.

**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0700B".
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
                   PROCEDURE  NYSFILF.
     MOVE   "NYSFILL1"        TO    ERR-FL-ID.
     MOVE    NYS-STATUS       TO    ERR-STCD.
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
     MOVE   "ZSOKMS1 "        TO    ERR-FL-ID.
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
                   PROCEDURE  SNDHIKF.
     MOVE   "SNDHIKF"        TO    ERR-FL-ID.
     MOVE    SND-STATUS       TO    ERR-STCD.
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

     OPEN     I-O       NYSFILF.
     OPEN     INPUT     SUBMEIF  ZSOKMS  HJYOKEN.
     OPEN     OUTPUT    SNDHIKF.
*条件ファイルにて廃棄用の顧客、納入先を取得
     MOVE  66           TO   JYO-F01.
     MOVE  "D365HIK"    TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = "INV"
         MOVE  4000     TO   PROGRAM-STATUS
         DISPLAY NC"＃＃条件Ｆ無し！！＃＃" UPON CONS
         DISPLAY NC"＃　固定値取得エラー＃" UPON CONS
         DISPLAY NC"＃　確認必要です。　＃" UPON CONS
         STOP  RUN
     ELSE
         MOVE  JYO-F14  TO   WK-HIK-KOKYAKU
         MOVE  JYO-F15  TO   WK-HIK-NOUNYU
         DISPLAY NC"顧客情報" " = " WK-HIK-KOKYAKU UPON CONS
         DISPLAY NC"納入情報" " = " WK-HIK-NOUNYU  UPON CONS
     END-IF.
*入出庫ファイルの読込
     PERFORM  NYSFILF-READ-SEC.

 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理
************************************************************
 200-MAIN-SEC           SECTION.

*サブ商品名称マスタ／検索
     MOVE     NYS-F05         TO   SUB-F011.    *> 商品ＣＤ
     MOVE     NYS-F06         TO   SUB-F012.    *> 品単ＣＤ
     PERFORM  SUBMEIF-READ-SEC.
     IF  SUBMEIF-INV-FLG = "INV"
         DISPLAY NC"＃ＳＵＢ商品名称Ｍ無！＃" UPON CONS
         DISPLAY NC"＃エラー商品ＣＤ" " = " SUB-F011 UPON CONS
         DISPLAY NC"＃エラー品単ＣＤ" " = " SUB-F012 UPON CONS
         MOVE  4000          TO   PROGRAM-STATUS
         STOP  RUN
     END-IF.

*廃棄実績データ／編集
     MOVE      SPACE         TO   SND-REC.
     INITIALIZE                   SND-REC.
*    Ｄ３６５伝票番号
     MOVE      "HK"          TO   SND-F01(1:2).
     MOVE      NYS-F15(3:6)  TO   SND-F01(3:6).
     MOVE      NYS-F02       TO   SND-F01(9:7).
     MOVE      "Z01"         TO   SND-F01(16:3).
*    オーダー区分
     MOVE      "Z01"         TO   SND-F04(1:3).
*    受注日
     MOVE      NYS-F15(1:4)  TO   SND-F05(1:4).
     MOVE      "/"           TO   SND-F05(5:1).
     MOVE      NYS-F15(5:2)  TO   SND-F05(6:2).
     MOVE      "/"           TO   SND-F05(8:1).
     MOVE      NYS-F15(7:2)  TO   SND-F05(9:2).
*    販売価格設定
     MOVE      "1"           TO   SND-F10.
*    倉庫　　　　　　　
     MOVE      NYS-F10       TO   SOK-F01.
     PERFORM   ZSOKMS-READ-SEC.
     IF   ZSOKMS-INV-FLG  =  "INV"
          MOVE "SSC"         TO   SND-F11
          MOVE "0"           TO   SND-F12(1:1)
          MOVE  NYS-F10      TO   SND-F12(2:2)
          MOVE  "11"         TO   SND-F13
     ELSE
**********サイト取得
          MOVE  41           TO   JYO-F01
          MOVE  SOK-F15      TO   JYO-F02
          PERFORM  HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = "INV"
              MOVE  "SSC"    TO   SND-F11
          ELSE
              MOVE   JYO-F14 TO   SND-F11
          END-IF
**********Ｄ３６５倉庫ＣＤ
          MOVE  SOK-F16      TO   SND-F12
**********保管場所取得
          MOVE  42           TO   JYO-F01
          MOVE  SOK-F17      TO   JYO-F02
          PERFORM  HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = "INV"
              MOVE  "11"     TO   SND-F13
          ELSE
              MOVE   JYO-F14 TO   SND-F13
          END-IF
     END-IF.
*    出荷予定日
     MOVE    "6"        TO        LINK-IN-KBN.
     MOVE     1         TO        LINK-IN-YMD6.
     MOVE     NYS-F15   TO        LINK-IN-YMD8.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   NOT =    ZERO
              MOVE    ZERO             TO   LINK-OUT-YMD
     END-IF.
     MOVE      LINK-OUT-YMD(1:4)  TO   SND-F14(1:4).
     MOVE      "/"           TO   SND-F14(5:1).
     MOVE      LINK-OUT-YMD(5:2)  TO   SND-F14(6:2).
     MOVE      "/"           TO   SND-F14(8:1).
     MOVE      LINK-OUT-YMD(7:2)  TO   SND-F14(9:2).
*    入荷希望日
     MOVE      NYS-F15(1:4)  TO   SND-F15(1:4).
     MOVE      "/"           TO   SND-F15(5:1).
     MOVE      NYS-F15(5:2)  TO   SND-F15(6:2).
     MOVE      "/"           TO   SND-F15(8:1).
     MOVE      NYS-F15(7:2)  TO   SND-F15(9:2).
*    顧客発注番号
     MOVE      NYS-F02       TO   SND-F18.
*    外部システム番号
     MOVE      SND-F01       TO   SND-F19.
*# 2020/07/20 NAV ED
*    連携ＮＯ
*    連携データ区分
     MOVE      "1"           TO   SND-F21.
*    顧客ＩＤ
     MOVE     WK-HIK-KOKYAKU TO   SND-F22.
*    旧顧客ＣＤ番号
*****MOVE     WK-HIK-KOKYAKU TO   SND-F23.
     MOVE     SPACE          TO   SND-F23.
*    需要家ＩＤ
     MOVE     WK-HIK-NOUNYU  TO   SND-F24.
*    旧納入先ＣＤ
*****MOVE      000057        TO   WK-TENPO-CD.
*****MOVE      WK-TENPO-HEN  TO   SND-F25.
*****MOVE     WK-HIK-NOUNYU  TO   SND-F25.
     MOVE     SPACE          TO   SND-F25.
*    一時出荷先フラグ
     MOVE      "0"           TO   SND-F26.
*    旧商品ＣＤ
*    ＪＡＮＣＤ
     MOVE      SUB-D01       TO   SND-F46.
*    数量符号
     MOVE      "0"           TO   SND-F470.
*    数量　　　　　
     MOVE      NYS-F12       TO   SND-F47.
     COMPUTE SND-F47 = NYS-F12 * 100.
*    直送フラグ
     MOVE      "0"           TO   SND-F51.
*    販売価格設定
     MOVE      "1"           TO   SND-F57.
*    販売単価
     MOVE      ZERO          TO   SND-F58.
*    受注金額符号
     MOVE      "0"           TO   SND-F590.
*    受注金額
     MOVE      ZERO          TO   SND-F59.
*    販売価格単位
     MOVE      "1"           TO   SND-F60.
*    需要家ＩＤ　　
     MOVE      SND-F24       TO   SND-F61.
*    旧納入先ＣＤ
*****MOVE      SND-F25       TO   SND-F62.
     MOVE      SPACE         TO   SND-F62.
*    出荷先フラグ
     MOVE      "0"           TO   SND-F63.
*    外部システム番号
     MOVE      SND-F01       TO   SND-F77.
*    外部システム行番号
     MOVE      NYS-F03       TO   SND-F78.
*    入出庫ファイル項目セット
     MOVE      1             TO   NYS-F14.
     MOVE      1             TO   NYS-F89.
     MOVE      WK-SYSDATE    TO   NYS-F90.
     MOVE      WK-SYSTIME    TO   NYS-F91.
     REWRITE   NYS-REC.
*
     WRITE  SND-REC.
     ADD       1             TO   OUT-CNT.
*次の入出庫ファイルの読込
     PERFORM  NYSFILF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    入出庫ファイルの読込処理
************************************************************
 NYSFILF-READ-SEC       SECTION.

 NYSFILF-READ-100.
     READ    NYSFILF   NEXT
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        NYSFILF-READ-EXT
     END-READ.

     ADD     1                   TO   IN-CNT.
     IF     IN-CNT-X  =  "500"  OR  "000"
            DISPLAY "READ-CNT = " IN-CNT-X  UPON CONS
     END-IF.

*Ｄ３６５計上区分＝空白が対象
     IF     NYS-F14  =  ZERO
            CONTINUE
     ELSE
            GO          TO       NYSFILF-READ-100
     END-IF.

*作業区分＝４６が対象
     IF     NYS-F04  =  "46"
            CONTINUE
     ELSE
            GO          TO       NYSFILF-READ-100
     END-IF.

     ADD     1                   TO   SEL-CNT.

*削除区分が１以外が対象
     IF     NYS-F96  =  "1"
            GO          TO       NYSFILF-READ-100
     END-IF.

 NYSFILF-READ-EXT.
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
*
     IF  OUT-CNT = ZERO
         DISPLAY NC"＃抽出対象がありません！＃" UPON CONS
     END-IF.
*
     MOVE   OUT-CNT              TO   PARA-CNT.

*#### TEST <<<<<<
*### DISPLAY "  " UPON CONS.
*### DISPLAY " 件数 ------- "  PARA-CNT  UPON CONS.
*#### TEST >>>>>>

     CLOSE  NYSFILF
            SUBMEIF  ZSOKMS  HJYOKEN
            SNDHIKF.
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
